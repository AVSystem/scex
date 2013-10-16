import com.avsystem.scex.AbstractExpressionContext
import com.avsystem.scex.compiler.{DefaultJavaScexCompiler, ExpressionProfile, ScexCompilerConfig}
import com.avsystem.scex.validation._
import java.util.Collections
import java.{util => ju, lang => jl}
import reflect.macros.Universe
import scala.Some
import scala.language.dynamics
import scala.language.existentials
import scala.runtime.RichInt


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

  object Dyn extends Dynamic {
    def selectDynamic(attr: String) = attr
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

      on { anyRef: AnyRef =>
        anyRef == (_: AnyRef)
        anyRef != (_: AnyRef)
      }

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
        new ju.ArrayList(_: ju.Collection[_])
        al.all.members
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

      Dyn.selectDynamic _

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

    //memberAccessSpecs foreach println

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

    val symbolValidator = SymbolValidator(memberAccessSpecs)

    val profile = new ExpressionProfile(syntaxValidator, symbolValidator, "", "val lol = \"dafuq\"; def immaUtil = \"util, lol\"")
    val compiler = new DefaultJavaScexCompiler(new ScexCompilerConfig)

    val myexpr = "(null: A[_])"

    val expr = """ Some((3, "50")) """

    class TL extends TypedLol[TL]

    val typedLol = new TL
    val dafuq = new typedLol.Dafuq[ju.ArrayList[CharSequence]]

    type Typ = TypedLol[T]#Dafuq[F] forSome {type T <: TypedLol[T]; type F}

    //compiler.getCompiledExpression(profile, "ValidationTest.Dyn.costam", classOf[Object], classOf[String])

    val ic = compiler.getInteractiveContext[AbstractExpressionContext[_, _], Object](profile)
    ic.getScopeCompletion("", 0).members foreach println

  }

}
