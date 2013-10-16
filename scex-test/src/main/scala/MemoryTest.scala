import com.avsystem.scex.compiler.{ExpressionProfile, ScexCompilerConfig}
import com.avsystem.scex.japi.DefaultJavaScexCompiler
import com.avsystem.scex.validation.{SyntaxValidator, SymbolValidator}
import com.avsystem.scex.{ExpressionContext, PredefinedAccessSpecs}
import java.{util => ju, lang => jl}


object MemoryTest {

  case class Dummy(costam: Int)

  def main(args: Array[String]) {
    val config = new ScexCompilerConfig
    config.expressionExpirationTime = 500

    val compiler = new DefaultJavaScexCompiler(config)

    import SymbolValidator._
    val symbolValidator = SymbolValidator(
      PredefinedAccessSpecs.basicOperations ++ allow {
        on { dummy: Dummy =>
          new Dummy(_)
          dummy.toString
        }
      }
    )

    val profile = new ExpressionProfile(SyntaxValidator.SimpleExpressions, symbolValidator, "", "")

    var i = 0
    while (true) {
      i += 1
      compiler.getCompiledExpression[ExpressionContext, String](profile, s"new MemoryTest.Dummy($i).toString")
      if (i % 10 == 0) {
        println(i)
      }
    }
  }
}
