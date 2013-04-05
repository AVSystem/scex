import com.avsystem.scex.validation._
import java.{util => ju, lang => jl}
import reflect.macros.Universe
import scala.reflect.runtime.{universe => ru}
import scala.Some
import scala.tools.nsc.interpreter.IMain
import scala.tools.nsc.Settings

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

    var a: T = ???

    def b(): T = ???
  }

  class B extends A[Int] {
    override def costam(buu: Int) = buu * 2
  }

  def main(args: Array[String]) {
    val settings = new Settings
    settings.usejavacp.value = true

    val repl = new IMain(settings)

    val codeTemplate = """
                         |import com.avsystem.scex.validation._
                         |
                         |val expr = { __ctx: Any =>
                         |  import __ctx._
                         |  Validator.validate({%s})
                         |}
                       """.stripMargin

    val myexpr =
      """
        |{
        |  String.CASE_INSENSITIVE_ORDER
        |  val b = new ValidationTest.B
        |  b.a = 42
        |  val jl = new JavaLol
        |  jl.fuu
        |  jl.fuu = 30
        |  new JavaLol + "fuu"
        |}
      """.stripMargin

    import com.avsystem.scex.validation.AccessValidators._

    Validator.accessValidator = ChainValidator({
      implicit def anythingImplicitly[T]: T = ???

      List(
        denyOn[Any] {
          any =>
            any.equals _
            any.hashCode
            any.##
            any.getClass
            any.asInstanceOf
            any.isInstanceOf
        },

        denyOn[Object] {
          anyRef =>
            anyRef.eq _
            anyRef.synchronized _
        },

        allowOn[Any] {
          any =>
            any + (_: String)
            any -> (_: Any)
            any == (_: Any)
            any != (_: Any)
        },

        allow {
          ValidationTest.Foo.Bar.c
          String.CASE_INSENSITIVE_ORDER
          Some.apply _
          String.valueOf(_: Boolean)
          new B
          new JavaLol
        },

        allowOn[String] {
          s =>
            s.length
            s.concat _
            s.matches _
            s.reverse
            s.compare(_: String)
        },

        allowOn[A[_]] {
          a =>
            a.costam _
            a.hoho _
            a.multiParens _
            a.b()
            a.getClass
            a.a_= _
        },

        allowOn[Int] {
          i =>
            i + (_: Char)
        },

        allowOn[JavaLol] {
          jl =>
            jl.fuu
        },

        denyAny
      )
    })

    Validator.accessValidator.asInstanceOf[ChainValidator].validators.collect {
      case v: TypeMembersValidator => v.typesAndMembers foreach println
    }

    Validator.syntaxValidator = new SyntaxValidator {
      def isSyntaxAllowed(u: Universe)(tree: u.Tree): Boolean = {
        import u._

        tree match {
          case _: Block | _: Select | _: Apply | _: TypeApply | _: Ident |
               _: If | _: Literal | _: New | _: This | _: Typed | _: TypTree => true
          case _ => false
        }
      }
    }

    repl interpret codeTemplate.format(myexpr)
  }

}
