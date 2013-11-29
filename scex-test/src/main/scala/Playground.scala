
import com.avsystem.scex.util.Literal
import java.{util => ju, lang => jl}
import scala.language.experimental.macros

object Playground {

  implicit class any2qmark[A](value: => A) {
    def ?[B](default: => B)(implicit atob: A => B): B =
      try {
        val result = value
        if (result != null) result else default
      } catch {
        case _: NullPointerException => default
      }
  }

  implicit class literal2qmark(value: Literal) {
    def ?[B](default: B)(implicit conv: Literal => B): B = {
      try {
        val result = value
        if (result != null && result.literalString != null) result else default
      } catch {
        case _: NullPointerException => default
      }
    }
  }

  class A

  implicit def aToBoolean(a: A) = true

  def main(args: Array[String]) {
    val costam: Boolean = math.random > 0.5
    val str = costam.toString
  }
}
