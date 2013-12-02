
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


}
