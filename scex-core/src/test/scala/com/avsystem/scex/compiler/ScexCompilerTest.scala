package com.avsystem.scex
package compiler

import com.avsystem.scex.compiler.ParameterizedClass.StaticInnerGeneric
import com.avsystem.scex.compiler.ScexCompiler.CompilationFailedException
import com.avsystem.scex.compiler.overriding.{Base, Klass, Specialized}
import com.avsystem.scex.util.{PredefinedAccessSpecs, SimpleContext}
import com.google.common.reflect.TypeToken
import org.scalatest.funsuite.AnyFunSuite

import java.{util => ju}
import scala.annotation.nowarn
import scala.collection.immutable.StringOps

object ScexCompilerTest {
  implicit final class JListExt[T](private val jlist: ju.List[T]) extends AnyVal {
    def apply(i: Int): T = jlist.get(i)
  }
}

@nowarn("msg=a pure expression does nothing in statement position")
class ScexCompilerTest extends AnyFunSuite with CompilationTest {

  import com.avsystem.scex.validation.SymbolValidator._

  override def defaultAcl = Nil

  test("trivial compilation test") {
    evaluate[Unit]("()")
  }

  test("simple arithmetic expression") {
    assert(298 == evaluate[Int]("1+5+250+42"))
  }

  test("simple syntax validation test") {
    intercept[CompilationFailedException] {
      evaluate[Unit]("while(true) {}")
    }
  }

  test("simple member validation test") {
    assertMemberAccessForbidden {
      evaluate[Unit]("System.exit(0)")
    }
  }

  test("syntax error test") {
    intercept[CompilationFailedException] {
      evaluate[Unit]("this doesn't make sense")
    }
  }

  test("root access test with java getter adapters") {
    val acl = PredefinedAccessSpecs.basicOperations ++ allow {
      on { jc: JavaRoot =>
        jc.all.members
      }
    }
    val expr = "property + extraordinary + extraordinarilyBoxed + field + twice(42)"
    val cexpr = compiler.getCompiledExpression[SimpleContext[JavaRoot], String](createProfile(acl), expr, template = false)
    assert("propertytruefalse42.4284" == cexpr(SimpleContext(new JavaRoot)))
  }

  test("complicated root type test with java getter adapters") {
    val acl = PredefinedAccessSpecs.basicOperations ++ allow {
      on { m: ju.Map[_, _] =>
        m.toString
      }
      on { ct: (ParameterizedClass.StaticInnerGeneric[A]#DeeplyInnerGeneric[B] forSome {type A; type B}) =>
        ct.all.members
      }
    }
    type RootType = ParameterizedClass.StaticInnerGeneric[Cloneable]#DeeplyInnerGeneric[_]
    val expr = """ "EXPR:" + awesomeness + sampleMap + handleStuff("interesting stuff") + awesome + fjeld """
    val cexpr = compiler.buildExpression.contextType(new TypeToken[SimpleContext[RootType]] {}).template(false)
      .resultType(classOf[String]).profile(createProfile(acl)).expression(expr).get

    val sig = new StaticInnerGeneric[Cloneable]
    assert("EXPR:true{}[interesting stuff handled]true[fjeld]" == cexpr(SimpleContext(new sig.DeeplyInnerGeneric[String])))
  }

  test("non-root java getter adapters test") {
    val acl = PredefinedAccessSpecs.basicOperations ++ allow {
      on { jc: JavaRoot =>
        new JavaRoot
        jc.all.members
      }
    }
    val expr = "new JavaRoot().property + new JavaRoot().extraordinary + new JavaRoot().extraordinarilyBoxed"
    val cexpr = compiler.getCompiledExpression[SimpleContext[Unit], String](createProfile(acl), expr, template = false)
    assert("propertytruefalse" == cexpr(SimpleContext(())))
  }

  test("String.empty") {
    val acl = PredefinedAccessSpecs.basicOperations
    assert(!evaluate[Boolean]("\"str\".empty", acl))
  }

  test("constructor allow test") {
    val acl = allow {
      new JavaRoot
    }
    val expr = "new JavaRoot"
    val cexpr = compiler.getCompiledExpression[SimpleContext[Unit], Unit](createProfile(acl), expr, template = false)
    cexpr(SimpleContext(()))
  }

  test("constructor deny test") {
    val acl = deny {
      new JavaRoot
    }
    val expr = "new JavaRoot"
    assertMemberAccessForbidden {
      compiler.getCompiledExpression[SimpleContext[Unit], Unit](createProfile(acl), expr, template = false)
    }
  }

  test("constructor not allow test") {
    val expr = "new JavaRoot"
    assertMemberAccessForbidden {
      compiler.getCompiledExpression[SimpleContext[Unit], Unit](createProfile(Nil), expr, template = false)
    }
  }

  test("java getter validation test") {
    val acl = allow {
      on { jc: JavaRoot =>
        jc.getProperty
      }
    }
    val expr = "property"
    val cexpr = compiler.getCompiledExpression[SimpleContext[JavaRoot], String](createProfile(acl), expr, template = false)
    assert("property" == cexpr(SimpleContext(new JavaRoot)))
  }

  test("validation test with subtyping") {
    val acl = deny {
      on { jc: JavaRoot =>
        jc.getProperty
      }
    } ++ allow {
      on { djc: DerivedJavaRoot =>
        djc.all.members
      }
    }
    val expr = "property"
    assertMemberAccessForbidden {
      compiler.getCompiledExpression[SimpleContext[DerivedJavaRoot], String](createProfile(acl), expr, template = false)
    }
  }

  test("validation test with overriding") {
    val acl = allow {
      on { jc: JavaRoot =>
        jc.overriddenMethod()
      }
    }
    val expr = "overriddenMethod()"
    val cexpr = compiler.getCompiledExpression[SimpleContext[DerivedJavaRoot], Unit](createProfile(acl), expr, template = false)
    cexpr(SimpleContext(new DerivedJavaRoot))
  }

  test("header test") {
    val acl = allow {
      new ju.ArrayList[String]
    }
    val expr = "new ArrayList[String]"
    val cexpr = compiler.getCompiledExpression[SimpleContext[Unit], ju.List[_]](
      createProfile(acl, header = "import java.util.ArrayList"), expr, template = false)
    assert(new ju.ArrayList[String] == cexpr(SimpleContext(())))
  }

  test("utils test") {
    val expr = "utilValue"
    val cexpr = compiler.getCompiledExpression[SimpleContext[Unit], Int](
      createProfile(Nil, header = "", utils = "val utilValue = 42"), expr, template = false)
    assert(42 == cexpr(SimpleContext(())))
  }

  test("implicit context test") {
    val expr = "gimmeVar(\"tehname\")"
    val acl = allow {
      on { car: ContextAccessingRoot =>
        car.gimmeVar(_: String)(_: ExpressionContext[ContextAccessingRoot, String])
      }
    }
    val cexpr = compiler.getCompiledExpression[SimpleContext[ContextAccessingRoot], String](createProfile(acl), expr, template = false)
    val ctx = SimpleContext(new ContextAccessingRoot)
    ctx.setVariable("tehname", "tehvalue")
    assert("tehvalue" == cexpr(ctx))
  }

  test("ACL allow entry precedence test") {
    val acl = allow {
      on { i: Int =>
        i.toString
      }
    } ++ deny {
      on { i: Int =>
        i.toString
      }
    }
    val expr = "42.toString"
    val cexpr = compiler.getCompiledExpression[SimpleContext[Unit], String](createProfile(acl), expr, template = false)
    assert("42" == cexpr(SimpleContext(())))
  }

  test("ACL deny entry precedence test") {
    val acl = deny {
      on { i: Int =>
        i.toString
      }
    } ++ allow {
      on { i: Int =>
        i.toString
      }
    }
    val expr = "42.toString"
    assertMemberAccessForbidden {
      compiler.getCompiledExpression[SimpleContext[Unit], String](createProfile(acl), expr, template = false)
    }
  }

  test("static module access validation test") {
    val acl = allow {
      Some.apply(_: Any)
    }
    val expr = "Some"
    assertMemberAccessForbidden {
      compiler.getCompiledExpression[SimpleContext[Unit], Unit](createProfile(acl), expr, template = false)
    }
  }

  test("static module member validation test 1") {
    val acl = allow {
      Some.apply(_: Any)
    }
    val expr = "Some(42)"
    val cexpr = compiler.getCompiledExpression[SimpleContext[Unit], Option[Int]](createProfile(acl), expr, template = false)
    assert(Some(42) == cexpr(SimpleContext(())))
  }

  test("static module member validation test 2") {
    val acl = allow {
      Some.all.members
    }
    val expr = "Some.hashCode"
    assertMemberAccessForbidden {
      compiler.getCompiledExpression[SimpleContext[Unit], Unit](createProfile(acl), expr, template = false)
    }
  }

  test("explicit member-by-implicit validation test 1") {
    val acl = allow {
      on { s: String =>
        s.reverse
      }
    }
    val expr = "\"bippy\".reverse"
    val cexpr = compiler.getCompiledExpression[SimpleContext[Unit], String](createProfile(acl), expr, template = false)
    assert("yppib" == cexpr(SimpleContext(())))
  }

  test("explicit member-by-implicit validation test 2") {
    import TestExtensions._
    val acl = allow {
      on { any: Any =>
        any ? (_: Any)
      }
    }
    val expr = "\"bippy\" ? \"fuu\""
    val cexpr = compiler.getCompiledExpression[SimpleContext[Unit], String](
      createProfile(acl, header = "import com.avsystem.scex.compiler.TestExtensions._"), expr, template = false)
    assert("bippy" == cexpr(SimpleContext(())))
  }

  test("plain implicit conversion validation test") {
    val acl = allow {
      Predef.augmentString _
      on { s: StringOps =>
        s.reverse
      }
    }
    val expr = "\"bippy\".reverse"
    val cexpr = compiler.getCompiledExpression[SimpleContext[Unit], String](createProfile(acl), expr, template = false)
    assert("yppib" == cexpr(SimpleContext(())))
  }

  test("mixed access implicit conversion validation test 1") {
    val acl = deny {
      Predef.augmentString _
    } ++ allow {
      on { s: String =>
        s.reverse
      }
    }
    val expr = "\"bippy\".reverse"
    assertMemberAccessForbidden {
      compiler.getCompiledExpression[SimpleContext[Unit], String](createProfile(acl), expr, template = false)
    }
  }

  test("mixed access implicit conversion validation test 2") {
    val acl = deny {
      on { s: StringOps =>
        s.reverse
      }
    } ++ allow {
      on { s: String =>
        s.reverse
      }
    }
    val expr = "\"bippy\".reverse"
    assertMemberAccessForbidden {
      compiler.getCompiledExpression[SimpleContext[Unit], String](createProfile(acl), expr, template = false)
    }
  }

  test("member by generic implicit conversion on existential type test") {
    import ScexCompilerTest._
    val acl = allow {
      on { l: ju.List[_] =>
        l.apply(_: Int)
      }
    }
    val header = "import com.avsystem.scex.compiler.ScexCompilerTest._"
    val expr = "_root(0)"
    val cexpr = compiler.getCompiledExpression[SimpleContext[ju.List[String]], Any](createProfile(acl, header = header), expr, template = false)
    val list = ju.Arrays.asList("0", "1", "2")
    assert("0" == cexpr(SimpleContext(list)))
  }

  test("covariance by @plus annotation test") {
    val acl = allow {
      on { l: ju.List[Any@plus] =>
        l.add(_: Any)
      }
    }
    val expr = "_root.add(\"string\")"
    val cexpr = compiler.getCompiledExpression[SimpleContext[ju.List[String]], Unit](createProfile(acl), expr, template = false)
    val list = new ju.ArrayList[String]
    cexpr(SimpleContext(list))
    assert("string" == list.get(0))
  }

  test("contravariance by @minus annotation test") {
    val acl = allow {
      on { l: ju.List[String@minus] =>
        l.get _
      }
    }
    val expr = "_root.get(0)"
    val cexpr = compiler.getCompiledExpression[SimpleContext[ju.List[Any]], Any](createProfile(acl), expr, template = false)
    val list = ju.Arrays.asList[Any]("cos")
    cexpr(SimpleContext(list))
    assert("cos" == list.get(0))
  }

  test("dynamic test") {
    val acl = allow {
      SomeDynamic.selectDynamic _
    }
    val expr = "com.avsystem.scex.compiler.SomeDynamic.dynamicProperty"
    val cexpr = compiler.getCompiledExpression[SimpleContext[Unit], String](createProfile(acl), expr, template = false)
    assert("dynamicProperty" == cexpr(SimpleContext(())))
  }

  test("java getter inherited from multiple bases test") {
    val acl = allow {
      on { sjr: SubRoot =>
        sjr.all.members
      }
    }
    val expr = "self.id"
    val cexpr = compiler.getCompiledExpression[SimpleContext[SubRoot], String](createProfile(acl), expr, template = false)
    assert("tehId" == cexpr(SimpleContext(new SubRoot)))
  }

  // https://groups.google.com/forum/#!topic/scala-user/IeD2siVXyss
  test("overriding problem test") {
    val acl = allow {
      on { base: Base =>
        base.get _
      }
    }
    val expr = "_root.get(\"lol\")"
    val cexpr = compiler.getCompiledExpression[SimpleContext[Klass], AnyRef](createProfile(acl), expr, template = false)
    val root = new Klass
    assert(root eq cexpr(SimpleContext(root)))
  }

  test("overridden java getter with bridge method test") {
    val acl = allow {
      on { s: Specialized =>
        s.all.members
      }
      on { s: String =>
        s + _
        s.toString
      }
    }
    val expr = "that + _root.that"
    val cexpr = compiler.getCompiledExpression[SimpleContext[Specialized], String](createProfile(acl), expr, template = false)
    assert("thatthat" == cexpr(SimpleContext(new Specialized)))
  }

  test("toString on any2stringadd validation test") {
    val acl = allow {
      on { any: Any =>
        any + (_: String)
      }
      on { s: String =>
        s + _
        s.toString
      }
    }
    val expr = "_root + \"\""
    assertMemberAccessForbidden {
      compiler.getCompiledExpression[SimpleContext[JavaRoot], String](createProfile(acl), expr, template = false)
    }
  }

  def lambdaTest(name: String, expr: String) =
    test(name) {
      val acl = allow {
        List.apply _
        on { l: List[Any] =>
          l.forall _
        }
        on { i: Int =>
          i > (_: Int)
        }
      }

      val cexpr = compiler.getCompiledExpression[SimpleContext[Unit], Boolean](createProfile(acl), expr, template = false)
      assert(cexpr(SimpleContext(())))
    }

  lambdaTest("regular lambda test", "List(1,2,3).forall(x => 5 > x)")
  lambdaTest("short lambda test", "List(1,2,3).forall(5 > _)")
  lambdaTest("eta-expanded lambda test", "List(1,2,3).forall(5.>)")

  test("default arguments") {
    val profile = createProfile(Nil, utils = "def hasDefs(int: Int = 42, str: String = \"fuu\") = s\"$int:$str\"")
    val expr = "hasDefs(str = \"omglol\")"
    val cexpr = compiler.getCompiledExpression[SimpleContext[Unit], String](profile, expr, template = false)
    assert(cexpr(SimpleContext(())) == "42:omglol")
  }

}
