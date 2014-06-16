
import com.avsystem.scex.compiler.ScexCompilerConfig
import com.avsystem.scex.japi.XmlFriendlyJavaScexCompiler
import com.avsystem.scex.{PredefinedAccessSpecs, ExpressionProfile}
import com.avsystem.scex.validation.{SymbolValidator, SyntaxValidator}
import java.{util => ju, lang => jl}
import scala.language.experimental.macros

object Playground {

  def main(args: Array[String]) {
    val compiler = new XmlFriendlyJavaScexCompiler(new ScexCompilerConfig)

    val symbolValidator = SymbolValidator(PredefinedAccessSpecs.basicOperations)
    val syntaxValidator = SyntaxValidator.SimpleExpressions

    val profile = new ExpressionProfile(syntaxValidator, symbolValidator, "", "")

    val expr = compiler.getCompiledExpression[SimpleContext[Unit], String](profile,
      """          asdfasd
        |jkfalsdjkl
        |
        |
        |${(null: String).toString}
        |
        |asdfasd
      """.stripMargin)

    println(expr(SimpleContext(null)))
  }

}
