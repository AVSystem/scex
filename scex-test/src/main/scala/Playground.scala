
import com.avsystem.scex.compiler.ExpressionMacroProcessor
import com.avsystem.scex.util.Literal
import com.avsystem.scex.{Setter, Macros}
import java.{util => ju, lang => jl}
import scala.beans.BeanProperty
import scala.language.experimental.macros

object Playground {

  def asSetter[T](expr: T): Setter[T] = macro ExpressionMacroProcessor.asSetter_impl[T]

  implicit class int2plusLiteral(val int: Int) extends AnyVal {
    def +(lit: Literal) = int + lit.toString
  }

  class WithSetter {
    @BeanProperty var properitas: String = "fuu"
  }

  def main(args: Array[String]) {
    val ws = new Array[Int](5)
    println(ws.toList)
    val setter = asSetter(ws(2))
    setter(1529)
    println(ws.toList)
  }
}
