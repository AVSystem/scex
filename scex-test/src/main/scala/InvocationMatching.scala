import com.avsystem.scex.validation.InvocationValidators
import java.{util => ju, lang => jl}
import scala.reflect.api.{Mirror, Universe, TypeCreator}
import scala.reflect.runtime.{universe => ru}

object InvocationMatching {

  class A[T] {
    def costam(buu: T): T = buu

    def hoho[U <: List[String]](cos: U) = ???

    def multiParens(a: Int)(b: String, c: Float)(implicit d: T): Unit = ???
  }

  class B extends A[Int] {
    override def costam(buu: Int) = buu * 2
  }

  import InvocationValidators._

  implicit def anythingImplicitly[T]: T = ???
  val matchers = List(
    allow {
      Some.apply _
      String.valueOf(_: Boolean)
    },

    allowOn[String] { s =>
      s.length
      s.concat _
      s.matches _
    },

    allowOn[B] { b =>
      b.costam _
      b.hoho _
      b.multiParens _
    },

    denyAny
  )

  def main(args: Array[String]) {
    matchers foreach println
  }
}
