import java.{lang => jl, util => ju}

import com.avsystem.scex.japi.XmlFriendlyJavaScexCompiler
import com.avsystem.scex.validation.{SymbolValidator, SyntaxValidator}
import com.avsystem.scex.{ExpressionProfile, PredefinedAccessSpecs}

/**
 * Created: 04-12-2013
 * Author: ghik
 */
object ExistentialCase {
  def main(args: Array[String]) {
    val compiler = new XmlFriendlyJavaScexCompiler

    val symbolValidator = SymbolValidator(PredefinedAccessSpecs.basicOperations)
    val syntaxValidator = SyntaxValidator.SimpleExpressions

    val profile = new ExpressionProfile("test", syntaxValidator, symbolValidator, "", "")

    val completion = compiler.getCompleter[SimpleContext[Unit], Int](profile, template = true)
      .getTypeCompletion("${'dafuq'.toInt}", 14)

    completion.members foreach println
  }
}
