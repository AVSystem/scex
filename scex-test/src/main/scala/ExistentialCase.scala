import com.avsystem.scex.compiler.ScexCompilerConfig
import com.avsystem.scex.japi.XmlFriendlyJavaScexCompiler
import com.avsystem.scex.validation.{SymbolValidator, SyntaxValidator}
import com.avsystem.scex.{PredefinedAccessSpecs, ExpressionProfile}
import java.{util => ju, lang => jl}

/**
 * Created: 04-12-2013
 * Author: ghik
 */
object ExistentialCase {
  def main(args: Array[String]) {


    val compiler = new XmlFriendlyJavaScexCompiler(new ScexCompilerConfig)

    val symbolValidator = SymbolValidator(PredefinedAccessSpecs.basicOperations)
    val syntaxValidator = SyntaxValidator.SimpleExpressions

    val profile = new ExpressionProfile(syntaxValidator, symbolValidator, "", "")

    val completion = compiler.getCompleter[SimpleContext[Unit], Unit](profile, template = false)
      .getTypeCompletion("#srsly.", 0)

    completion.errors foreach println

    completion.members foreach println
  }
}
