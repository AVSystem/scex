import com.avsystem.scex.compiler.ScexSettings
import com.avsystem.scex.japi.DefaultJavaScexCompiler
import com.avsystem.scex.presentation.SymbolAttributes
import com.avsystem.scex.util.{PredefinedAccessSpecs, SimpleContext}
import com.avsystem.scex.validation.{SymbolValidator, SyntaxValidator}
import com.avsystem.scex.{ExpressionProfile, NamedSource}

import scala.annotation.nowarn

@nowarn("msg=a pure expression does nothing in statement position")
object MemoryTest {

  final case class Dummy(costam: Int)

  def main(args: Array[String]): Unit = {
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
    val symbolAttributes = SymbolAttributes(Nil)

    val profile = new ExpressionProfile("test", SyntaxValidator.SimpleExpressions, symbolValidator,
      symbolAttributes, "", NamedSource("test", ""))

    var i = 0
    while (true) {
      i += 1
      compiler.getCompiledExpression[SimpleContext[Any], String](profile, s"new MemoryTest.Dummy($i).toString")
      if (i % 10 == 0) {
        println(i)
      }
    }
  }
}
