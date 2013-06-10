import com.avsystem.scex.compiler.{ExpressionProfile, ScexCompiler, ScexCompilerConfig}
import com.avsystem.scex.validation._
import java.util.Collections
import java.{util => ju, lang => jl}
import reflect.macros.Universe
import scala.runtime.{StringAdd, RichInt}
import scala.Some
import scala.language.existentials


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
      allStatic[String].members
      Collections.emptyList
      new B
      new JavaLol
      None
      Tuple2.apply _

      on { tl: TypedLol[_] =>
        tl.toString
      }

      on { d: (TypedLol[T]#Dafuq[_] forSome {type T}) =>
        d.getStuff
      }

      on { s: String =>
        s.all.introduced.members
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
        new ju.ArrayList(_: ju.Collection[_])
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
        i.implicitlyAs[RichInt].all.membersNamed.to
        i.all.constructors
        i.all.membersNamed("+")
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

    val profile = new ExpressionProfile(syntaxValidator, symbolValidator, "def immaUtil = \"util, lol\"", "")
    val compiler = new ScexCompiler(new ScexCompilerConfig)

    val myexpr =
      """
        |{
        |  stuff
        |  null: String
        |  new java.util.ArrayList[String](java.util.Collections.emptyList)
        |  String.CASE_INSENSITIVE_ORDER
        |  new ValidationTest.B
        |  (new JavaLol).foo
        |  new JavaLol + s"fuu ${new JavaLol}"
        |  immaUtil
        |  None.hashCode
        |  Some((3, "50"))
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
