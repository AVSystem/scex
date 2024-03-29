import com.avsystem.scex.compiler.ScexSettings
import com.avsystem.scex.japi.XmlFriendlyJavaScexCompiler
import com.avsystem.scex.presentation.SymbolAttributes
import com.avsystem.scex.util.{PredefinedAccessSpecs, SimpleContext}
import com.avsystem.scex.validation.{SymbolValidator, SyntaxValidator}
import com.avsystem.scex.{ExpressionProfile, NamedSource}

/**
 * Created: 04-12-2013
 * Author: ghik
 */
object ExistentialCase {
  def main(args: Array[String]): Unit = {
    val compiler = new XmlFriendlyJavaScexCompiler(new ScexSettings)

    val symbolValidator = SymbolValidator(PredefinedAccessSpecs.basicOperations)
    val syntaxValidator = SyntaxValidator.SimpleExpressions
    val symbolAttributes = SymbolAttributes(Nil)

    val profile = new ExpressionProfile("test", syntaxValidator, symbolValidator, symbolAttributes, "", NamedSource("test", ""))

    val completion = compiler.getCompleter[SimpleContext[Unit], Int](profile, template = true)
      .getTypeCompletion("${'dafuq'.toInt}", 14)

    completion.members foreach println
  }
}
