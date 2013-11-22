
import com.avsystem.scex.util.Literal
import java.{util => ju, lang => jl}

object Playground {

  implicit class int2plusLiteral(val int: Int) extends AnyVal {
    def +(lit: Literal) = int + lit.toString
  }

  def main(args: Array[String]) {

    implicit class intToPlusLiteral(int: Int) {
      def +(lit: Literal) = int + lit.toInt
    }

    println(Literal("58") + Literal("42").toInt)

  }
}
