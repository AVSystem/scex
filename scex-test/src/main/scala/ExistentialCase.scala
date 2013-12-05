import com.avsystem.scex.ExpressionProfile
import com.avsystem.scex.compiler.{DefaultScexCompiler, ScexCompilerConfig}
import com.avsystem.scex.validation.SymbolValidator.MemberAccessSpec
import com.avsystem.scex.validation.SyntaxValidator
import java.{util => ju, lang => jl}

/**
 * Created: 04-12-2013
 * Author: ghik
 */
object ExistentialCase {
  def main(args: Array[String]) {
    val compiler = new DefaultScexCompiler(new ScexCompilerConfig)

    val symbolValidator = compiler.compileSymbolValidator(
      """
        |import com.avsystem.scex.validation.SymbolValidator._
        |
        |allow {
        |  on { s: Set[_] =>
        |    s.all.membersNamed.contains
        |  }
        |
        |  on { b: Boolean => b.all.members }
        |
        |  scala.collection.immutable.Set.apply _
        |}
      """.stripMargin)

    symbolValidator.accessSpecs foreach println

    val u = scala.reflect.runtime.universe
    symbolValidator.accessSpecs.map {
      case MemberAccessSpec(ti, _, _, _) => println(ti.typeIn(u))
    }

    val syntaxValidator = SyntaxValidator.SimpleExpressions

    val profile = new ExpressionProfile(syntaxValidator, symbolValidator, "", "")

    val completion = compiler.getCompleter[SimpleContext[Unit], Unit](profile, template = false)
      .getTypeCompletion("Set(50).contains(20)", 7)

    completion.members foreach println
  }
}
