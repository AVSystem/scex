import com.avsystem.scex.compiler.{ExpressionProfile, ExpressionCompiler, ExpressionCompilerConfig}
import com.avsystem.scex.validation._
import java.{util => ju, lang => jl}
import reflect.macros.Universe
import scala.Some
import scala.language.existentials
import scala.reflect.runtime.{universe => ru}

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

      on { tl: TypedLol[_] =>
        tl.toString
      }

      on { d: (TypedLol[T]#Dafuq[_] forSome {type T}) =>
        d.getStuff
      }

      on { s: String =>
        s.length
        s.concat _
        s.matches _
        s.reverse
        s.compare(_: String)
      }

      on { sc: StringContext =>
        sc.s _
      }

      on { al: ju.ArrayList[_] =>
        al.constructorWithSignature("(x$1: java.util.Collection[_ <: E])java.util.ArrayList[E]")
      }

      on { any: Any =>
        any + (_: String)
        any -> (_: Any)
        any == (_: Any)
        any != (_: Any)
      }

      on { a: A[_] =>
        a.costam _
        a.hoho _
        a.b()
        a.multiParens(_: Int)(_: String, _: Float)(_: Nothing)
        a.getClass
        a.a_= _
      }

      on { i: Int =>
        i.anyConstructor
        i.anyMethodNamed("+")
      }

      on { jl: JavaLol =>
        jl.fuu
        jl.isFoo
      }

    } ++ deny {

      on { any: Any =>
        any.equals _
        any.hashCode
        any.##
        any.getClass
        any.asInstanceOf
        any.isInstanceOf
      }

      on { anyRef: AnyRef =>
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
    val compiler = new ExpressionCompiler(new ExpressionCompilerConfig)

    val myexpr =
      """
        |{
        |  stuff
        |  null: String
        |  new java.util.ArrayList[String](null)
        |  String.CASE_INSENSITIVE_ORDER
        |  new ValidationTest.B
        |  (new JavaLol).foo
        |  com.avsystem.scex.util.CommonUtils.toString
        |  new JavaLol + s"fuu ${new JavaLol}"
        |}
      """.stripMargin

    class TL extends TypedLol[TL]

    val typedLol = new TL
    val dafuq = new typedLol.Dafuq[ju.ArrayList[CharSequence]]

    type Typ = TypedLol[T]#Dafuq[F] forSome {type T; type F}

    println(compiler.getCompiledExpression[Typ, Object](profile, myexpr, classOf[Typ], classOf[Object]).apply(dafuq))
    println(compiler.getCompiledStringExpression[Object](profile, "${1+5+10} hahaha \" dafuq \"", classOf[Object]).apply(null))
  }

}