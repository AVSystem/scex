import com.avsystem.scex.compiler.ScexSettings
import com.avsystem.scex.japi.XmlFriendlyJavaScexCompiler
import com.avsystem.scex.presentation.SymbolAttributes
import com.avsystem.scex.util.{SimpleContext, PredefinedAccessSpecs}
import com.avsystem.scex.validation.{SymbolValidator, SyntaxValidator}
import com.avsystem.scex.{ExpressionProfile, NamedSource}

/**
 * Created: 04-11-2014
 * Author: ghik
 */
object TemplateOptimizingPerformanceTest {
  def main(args: Array[String]): Unit = {
    val settings = new ScexSettings
    settings.classfileDirectory.value = "scex_classes"
    val compiler = new XmlFriendlyJavaScexCompiler(settings)

    val profile = new ExpressionProfile("test",
      SyntaxValidator.SimpleExpressions,
      SymbolValidator(PredefinedAccessSpecs.basicOperations),
      SymbolAttributes(Nil),
      "",
      NamedSource("empty", ""))

    val ctx = SimpleContext(())
    compiler.getCompiledExpression[SimpleContext[Unit], String](profile, "${1+2}abc").apply(ctx)

    Thread.sleep(5000)

    val start = System.nanoTime()
    var i = 0
    while (i < 200000) {
      val expr = "${1+2+3+4}" + i
      compiler.getCompiledExpression[SimpleContext[Unit], String](profile, expr).apply(ctx)
      i += 1
    }
    val duration = System.nanoTime() - start
    println(duration / 1000000)
  }
}
