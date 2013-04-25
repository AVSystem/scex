import com.avsystem.scex.{Utils, ExpressionProfile, ExpressionCompiler}
import com.avsystem.scex.validation._
import java.{util => ju, lang => jl}
import reflect.macros.Universe
import scala.reflect.runtime.{universe => ru}
import scala.Some

object ValidationTest {

  object Foo {

    object Bar {
      val c = 5
    }

  }

  class A[T] {
    def costam(buu: T): T = buu

    def hoho[U <: List[String]](cos: U) = ???

    def multiParens(a: Int)(b: String, c: Float)(implicit d: T): Unit = ???

    var a: T = _

    def b(): T = ???
  }

  class B extends A[Int] {
    override def costam(buu: Int) = buu * 2
  }

  def main(args: Array[String]) {

    import com.avsystem.scex.validation.SymbolValidator._

    val memberAccessSpecs = allow {
      StringContext.apply _
      ValidationTest.Foo.Bar.c
      String.CASE_INSENSITIVE_ORDER
      Some.apply _
      String.valueOf(_: Boolean)
      new B
      new JavaLol
      Utils.toString

      s: String => {
        s.length
        s.concat _
        s.matches _
        s.reverse
        s.compare(_: String)
      }

      sc: StringContext => {
        sc.s _
      }

      any: Any => {
        any + (_: String)
        any -> (_: Any)
        any == (_: Any)
        any != (_: Any)
      }

      a: A[_] => {
        a.costam _
        a.hoho _
        a.b()
        a.multiParens(_: Int)(_: String, _: Float)(_: Nothing)
        a.getClass
        a.a_= _
      }

      i: Int => {
        i + (_: Char)
      }

      jl: JavaLol => {
        jl.fuu
        jl.isFoo
      }

    } ++ deny {

      any: Any => {
        any.equals _
        any.hashCode
        any.##
        any.getClass
        any.asInstanceOf
        any.isInstanceOf
      }

      anyRef: AnyRef => {
        anyRef.eq _
        anyRef.synchronized _
      }

    }

    memberAccessSpecs foreach println

    val syntaxValidator = new SyntaxValidator {
      def isSyntaxAllowed(u: Universe)(tree: u.Tree): Boolean = {
        import u._

        tree match {
          case _: Block | _: Select | _: Apply | _: TypeApply | _: Ident |
               _: If | _: Literal | _: New | _: This | _: Typed | _: TypTree => true
          case _ => false
        }
      }
    }

    val symbolValidator = new SymbolValidator(memberAccessSpecs)

    val profile = new ExpressionProfile(syntaxValidator, symbolValidator, null)
    val compiler = new ExpressionCompiler

    val myexpr =
      """
        |{
        |  String.CASE_INSENSITIVE_ORDER
        |  new ValidationTest.B
        |  (new JavaLol).isFoo
        |  com.avsystem.scex.Utils.toString
        |  new JavaLol + s"fuu ${new JavaLol}"
        |}
      """.stripMargin

    println(compiler.getCompiledExpression[Object, Object](profile, myexpr).apply(null))
    println(compiler.getCompiledStringExpression[Object](profile, "${1+5+10} hahaha \" dafuq \"").apply(null))
  }

}