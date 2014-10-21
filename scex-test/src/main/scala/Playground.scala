
import java.{lang => jl, util => ju}

import com.avsystem.scex.japi.XmlFriendlyJavaScexCompiler
import com.avsystem.scex.validation.{SymbolValidator, SyntaxValidator}
import com.avsystem.scex.{ExpressionProfile, PredefinedAccessSpecs}

import scala.language.experimental.macros

object Playground {

  def main(args: Array[String]) {
    val compiler = new XmlFriendlyJavaScexCompiler
    compiler.settings.usejavacp.value = false
    compiler.settings.classpath.value = System.getProperty("java.class.path")
    compiler.settings.classpath.append("/home/ghik/testcp")

    compiler.compileClass(
      """
        |import com.avsystem.lol.Wutf
        |
        |class Klass {
        |  println(Wutf.srsly)
        |}
        |
      """.stripMargin, "Klass")
  }

}
