import com.avsystem.scex.compiler.{ExpressionProfile, ScexCompilerConfig, ScexCompiler}
import com.avsystem.scex.validation.{SyntaxValidator, SymbolValidator}
import java.{util => ju, lang => jl}
import scala.reflect.runtime.{universe => ru}

object MemoryTest {

  case class Dummy(costam: Int)

  def main(args: Array[String]) {
    val config = new ScexCompilerConfig
    config.expressionExpirationTime = 500

    val compiler = new ScexCompiler(config)

    import SymbolValidator._
    val symbolValidator = new SymbolValidator(
      allow {
        on { dummy: Dummy =>
          new Dummy(_)
          dummy.toString
        }
      }
    )

    val profile = new ExpressionProfile(SyntaxValidator.SimpleExpressions, symbolValidator, "")

    var i = 0
    while (true) {
      i += 1
      compiler.getCompiledExpression[Dummy, String](profile, s"new MemoryTest.Dummy($i).toString", classOf[Dummy], classOf[String])
      if (i % 10 == 0) {
        println(i)
      }
    }
  }
}
