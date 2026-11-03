package com.avsystem.scex.compiler.presentation

import com.avsystem.scex.compiler.presentation.ScopeAndTypeCompletionTest.Root
import com.avsystem.scex.compiler.{CompilationTest, ScexSettings}
import com.avsystem.scex.japi.{DefaultJavaScexCompiler, JavaScexCompiler}
import com.avsystem.scex.util.SimpleContext
import org.scalatest.funsuite.AnyFunSuite

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicInteger
import scala.jdk.CollectionConverters._
import scala.tools.nsc.Settings

/** Background compile resets a stale unit (clearing its synthetics) before re-parsing and re-entering it. If the
  * presentation compiler polled for work in between, a scex request could force an accessor symbol whose synthetic tree
  * is already gone ("no synthetic tree found for bean accessor"). This test fails deterministically if any polling
  * happens while a unit is being parsed and entered, regardless of whether the race actually triggers.
  */
class ParsePollingTest extends AnyFunSuite with CompilationTest {

  private val pollCount = new AtomicInteger
  private val pollsDuringParseAndEnter = new ConcurrentLinkedQueue[String]

  override protected def createCompiler: JavaScexCompiler = {
    val settings = new ScexSettings
    settings.classfileDirectory.value = "testClassfileCache"
    new DefaultJavaScexCompiler(settings) {
      override protected def createIGlobal(settings: Settings, reporter: Reporter, classLoader: ClassLoader) =
        new IGlobal(settings, reporter, classLoader) {
          override protected def checkForMoreWork(pos: Position): Unit = {
            pollCount.incrementAndGet()
            // Relies on scalac's `scala.tools.nsc.interactive.Global.parseAndEnter` showing up as a stack frame. If a
            // scalac upgrade renames or inlines it, this never matches and the test passes vacuously (pollCount only
            // proves this IGlobal is in use), so re-check the frame name after upgrading Scala.
            val inParseAndEnter = StackWalker.getInstance.walk { frames =>
              frames.anyMatch(f =>
                f.getClassName == "scala.tools.nsc.interactive.Global" && f.getMethodName == "parseAndEnter"
              )
            }
            if (inParseAndEnter) {
              pollsDuringParseAndEnter.add(Thread.currentThread.getStackTrace.mkString("\n  "))
            }
            super.checkForMoreWork(pos)
          }
        }
    }
  }

  val utils =
    """
      |def utilMethod: Int = 42
      |val utilStuff = 5
    """.stripMargin

  val profile = createProfile(utils = utils)

  test("presentation compiler does not poll for work while parsing and entering a unit") {
    val completer = compiler.getCompleter[SimpleContext[Root], Any](profile, template = false)
    completer.getScopeCompletion
    completer.getTypeCompletion("_root.", 5)

    assert(pollCount.get > 0, "custom IGlobal was not used")
    assert(pollsDuringParseAndEnter.isEmpty, pollsDuringParseAndEnter.asScala.headOption.getOrElse(""))
  }
}
