import java.{lang => jl, util => ju}

import com.avsystem.scex.compiler.ScexSettings
import com.avsystem.scex.japi.DefaultJavaScexCompiler
import com.avsystem.scex.validation.{SymbolValidator, SyntaxValidator}
import com.avsystem.scex.{NamedSource, ExpressionContext, ExpressionProfile, PredefinedAccessSpecs}


object MemoryTest {

  case class Dummy(costam: Int)

  def main(args: Array[String]) {
    val compiler = new DefaultJavaScexCompiler(new ScexSettings)
    compiler.settings.expressionExpirationTime.value = 500
    compiler.settings.resetAfterCount.value = 10

    import com.avsystem.scex.validation.SymbolValidator._
    val symbolValidator = SymbolValidator(
      PredefinedAccessSpecs.basicOperations ++ allow {
        on { dummy: Dummy =>
          new Dummy(_)
          dummy.toString
        }
      }
    )

    val profile = new ExpressionProfile("test", SyntaxValidator.SimpleExpressions, symbolValidator, "", NamedSource("test", ""))

    var i = 0
    while (true) {
      i += 1
      compiler.getCompiledExpression[ExpressionContext[Any, Any], String](profile, s"new MemoryTest.Dummy($i).toString")
      if (i % 10 == 0) {
        println(i)
      }
    }
  }
}
