
import java.{lang => jl, util => ju}

import com.avsystem.scex.compiler.{ScexGlobal, ScexSettings}
import com.avsystem.scex.japi.XmlFriendlyJavaScexCompiler
import com.avsystem.scex.validation.{SymbolValidator, SyntaxValidator}
import com.avsystem.scex.{ExpressionProfile, PredefinedAccessSpecs}

import scala.language.experimental.macros
import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.Global
import scala.tools.nsc.reporters.ConsoleReporter

object Playground {

  def main(args: Array[String]) {
    val settings = new ScexSettings
    settings.classpath.value = System.getProperty("java.class.path")
    val reporter = new ConsoleReporter(settings)
    val global = new Global(settings, reporter) with ScexGlobal

    import global._

    val run = new Run
    val tree = parseExpression("1+2+\"dafuq\".toString().fuqlolda[Abc]", template = false)
    println(showRaw(tree))

    global.currentRun

    val run2 = new Run
    run2.compileSources(List(new BatchSourceFile("dafuq.scala", "object o")))
  }

}
