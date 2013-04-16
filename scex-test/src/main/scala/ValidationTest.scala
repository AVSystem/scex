import com.avsystem.scex.{ExpressionProfile, ExpressionCompiler}
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

    val stuff = allow {
      StringContext.apply _
      ValidationTest.Foo.Bar.c
      String.CASE_INSENSITIVE_ORDER
      Some.apply _
      String.valueOf(_: Boolean)
      new B
      new JavaLol

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
    } ++ deny { any: Any =>
      any.getClass
      any.hashCode
    }

    stuff foreach println

//    val accessValidator = ChainValidator({
//      implicit def anythingImplicitly[T]: T = ???
//
//      List(
//        denyOn[Any] { any =>
//          any.equals _
//          any.hashCode
//          any.##
//          any.getClass
//          any.asInstanceOf
//          any.isInstanceOf
//        },
//
//        denyOn[Object] { anyRef =>
//          anyRef.eq _
//          anyRef.synchronized _
//        },
//
//        allowOn[Any] { any =>
//          any + (_: String)
//          any -> (_: Any)
//          any == (_: Any)
//          any != (_: Any)
//        },
//
//        allowOn[StringContext] { sc =>
//          sc.s _
//        },
//
//        allow {
//          StringContext.apply _
//          ValidationTest.Foo.Bar.c
//          String.CASE_INSENSITIVE_ORDER
//          Some.apply _
//          String.valueOf(_: Boolean)
//          new B
//          new JavaLol
//        },
//
//        allowOn[String] {
//          s =>
//            s.length
//            s.concat _
//            s.matches _
//            s.reverse
//            s.compare(_: String)
//        },
//
//        allowOn[A[_]] {
//          a =>
//            a.costam _
//            a.hoho _
//            a.multiParens _
//            a.b()
//            a.getClass
//            a.a_= _
//        },
//
//        allowOn[Int] {
//          i =>
//            i + (_: Char)
//        },
//
//        allowOn[JavaLol] {
//          jl =>
//            jl.fuu
//            jl.isFoo
//        },
//
//        denyAny
//      )
//    })

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

    val symbolValidator = new SymbolValidator

    val profile = new ExpressionProfile(syntaxValidator, symbolValidator, List(classOf[JavaLol]), null)
    val compiler = new ExpressionCompiler

    val myexpr =
      """
        |{
        |  String.CASE_INSENSITIVE_ORDER
        |  new ValidationTest.B
        |  (new JavaLol).foo
        |  new JavaLol + s"fuu ${new JavaLol}"
        |}
      """.stripMargin

    println(compiler.getCompiledStringExpression[Object](profile, "${1+5+10} hahaha \" dafuq \"").apply(null))
  }

}