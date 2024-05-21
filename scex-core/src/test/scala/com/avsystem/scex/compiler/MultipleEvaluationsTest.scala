package com.avsystem.scex.compiler

import com.avsystem.scex.compiler.presentation.TypeCompletionPrefixTest.Root
import com.avsystem.scex.japi.{DefaultJavaScexCompiler, JavaScexCompiler}
import com.avsystem.scex.util.SimpleContext
import org.scalatest.funsuite.AnyFunSuite

final class MultipleEvaluationsTest extends AnyFunSuite with CompilationTest {
  override protected def createCompiler: JavaScexCompiler = {
    val settings = new ScexSettings
    settings.classfileDirectory.value = "testClassfileCache"

    // for some reason enabling this setting fixes validation
    settings.noGetterAdapters.value = false
    new DefaultJavaScexCompiler(settings)
  }

  private val completer = compiler.getCompleter[SimpleContext[Root], Any](
    createProfile(Nil),
    template = false,
    header = "",
  )

  test("Multiple evaluations of invalid expressions should always fail") {
    (0 until 1000)
      .map(i => s"invalidExpression $i") //added to avoid hitting the cache
      .foreach(expr => if (completer.getErrors(expr).isEmpty) fail(s"Expression $expr should not compile"))
  }
}
