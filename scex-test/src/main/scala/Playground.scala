
import java.{lang => jl, util => ju}

import com.avsystem.scex.compiler.ScexSettings
import com.avsystem.scex.japi.XmlFriendlyJavaScexCompiler
import com.avsystem.scex.validation.{SymbolValidator, SyntaxValidator}
import com.avsystem.scex.{ExpressionProfile, PredefinedAccessSpecs}

import scala.language.experimental.macros

object Playground {

  def main(args: Array[String]) {
    val settings = new ScexSettings
    settings.usejavacp.value = false
    settings.classpath.value = System.getProperty("java.class.path")
    settings.classpath.append("/home/ghik/testcp")

    val compiler = new XmlFriendlyJavaScexCompiler(settings)

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
