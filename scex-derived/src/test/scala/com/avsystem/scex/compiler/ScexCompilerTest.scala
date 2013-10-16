package com.avsystem.scex.compiler

import com.avsystem.scex.compiler.ParameterizedClass.StaticInnerGeneric
import com.avsystem.scex.compiler.ScexCompiler.CompilationFailedException
import com.avsystem.scex.validation.{SymbolValidator, SyntaxValidator}
import com.avsystem.scex.PredefinedAccessSpecs
import java.{util => ju, lang => jl}
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import scala.collection.immutable.StringOps
import com.google.common.reflect.TypeToken

@RunWith(classOf[JUnitRunner])
class ScexCompilerTest extends FunSuite {

  import SymbolValidator._

  val compiler = new DefaultJavaScexCompiler(new ScexCompilerConfig)

  def catchAndPrint(code: => Any) {
    try code catch {
      case t: Throwable => t.printStackTrace(System.out)
    }
  }

  def createProfile(acl: List[MemberAccessSpec], header: String = "import com.avsystem.scex.compiler._", utils: String = "") =
    new ExpressionProfile(SyntaxValidator.SimpleExpressions, SymbolValidator(acl), header, utils)

  def assertMemberAccessForbidden(expr: => Any) {
    try expr catch {
      case e: CompilationFailedException =>
        assert(e.errors.forall(_.msg.startsWith("Member access forbidden")))
    }
  }

  test("trivial compilation test") {
    val cexpr = compiler.getCompiledExpression[SimpleContext[Unit], Unit](createProfile(Nil), "()")
    assert(() === cexpr(SimpleContext(())))
  }

  test("simple arithmetic expression") {
    val cexpr = compiler.getCompiledExpression[SimpleContext[Unit], Int](createProfile(Nil), "1+5+250+42")
    assert(298 === cexpr(SimpleContext(())))
  }

  test("strip dollar and brackets test") {
    val cexpr = compiler.getCompiledExpression[SimpleContext[Unit], Int](createProfile(Nil), "${1+5+250+42}")
    assert(298 === cexpr(SimpleContext(())))
  }

  test("string expression test") {
    val expr = "bippy \"${42/7}\" rest"
    val cexpr = compiler.getCompiledStringExpression[SimpleContext[Unit]](createProfile(PredefinedAccessSpecs.basicOperations), expr)
    assert("bippy \"6\" rest" === cexpr(SimpleContext(())))
  }

  test("simple syntax validation test") {
    intercept[CompilationFailedException] {
      compiler.getCompiledExpression[SimpleContext[Unit], Unit](createProfile(Nil), "while(true) {}")
    }
  }

  test("simple member validation test") {
    assertMemberAccessForbidden {
      compiler.getCompiledExpression[SimpleContext[Unit], Unit](createProfile(Nil), "System.exit(0)")
    }
  }

  test("syntax error test") {
    intercept[CompilationFailedException] {
      compiler.getCompiledExpression[SimpleContext[Unit], Unit](createProfile(Nil), "this doesn't make sense")
    }
  }

  test("root access test with java getter adapters") {
    val acl = PredefinedAccessSpecs.basicOperations ++ allow {
      on { jc: JavaRoot =>
        jc.all.members
      }
    }
    val expr = "property + extraordinary + extraordinarilyBoxed + field + twice(42)"
    val cexpr = compiler.getCompiledExpression[SimpleContext[JavaRoot], String](createProfile(acl), expr)
    assert("propertytruefalse42.4284" === cexpr(SimpleContext(new JavaRoot)))
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
    val cexpr = compiler.getCompiledExpression(createProfile(acl), expr,
      new TypeToken[SimpleContext[RootType]] {}, classOf[String])
    val sig = new StaticInnerGeneric[Cloneable]
    assert("EXPR:true{}[interesting stuff handled]true[fjeld]" === cexpr(SimpleContext(new sig.DeeplyInnerGeneric[String])))
  }

  test("non-root java getter adapters test") {
    val acl = PredefinedAccessSpecs.basicOperations ++ allow {
      on { jc: JavaRoot =>
        new JavaRoot
        jc.all.members
      }
    }
    val expr = "new JavaRoot().property + new JavaRoot().extraordinary + new JavaRoot().extraordinarilyBoxed"
    val cexpr = compiler.getCompiledExpression[SimpleContext[Unit], String](createProfile(acl), expr)
    assert("propertytruefalse" === cexpr(SimpleContext(())))
  }

  test("constructor allow test") {
    val acl = allow {
      new JavaRoot
    }
    val expr = "new JavaRoot"
    val cexpr = compiler.getCompiledExpression[SimpleContext[Unit], Unit](createProfile(acl), expr)
    assert(() === cexpr(SimpleContext(())))
  }

  test("constructor deny test") {
    val acl = deny {
      new JavaRoot
    }
    val expr = "new JavaRoot"
    assertMemberAccessForbidden {
      compiler.getCompiledExpression[SimpleContext[Unit], Unit](createProfile(acl), expr)
    }
  }

  test("constructor not allow test") {
    val expr = "new JavaRoot"
    assertMemberAccessForbidden {
      compiler.getCompiledExpression[SimpleContext[Unit], Unit](createProfile(Nil), expr)
    }
  }

  test("java getter validation test") {
    val acl = allow {
      on { jc: JavaRoot =>
        jc.getProperty
      }
    }
    val expr = "property"
    val cexpr = compiler.getCompiledExpression[SimpleContext[JavaRoot], String](createProfile(acl), expr)
    assert("property" === cexpr(SimpleContext(new JavaRoot)))
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
      compiler.getCompiledExpression[SimpleContext[DerivedJavaRoot], String](createProfile(acl), expr)
    }
  }

  test("validation test with overriding") {
    val acl = allow {
      on { jc: JavaRoot =>
        jc.overriddenMethod()
      }
    }
    val expr = "overriddenMethod()"
    val cexpr = compiler.getCompiledExpression[SimpleContext[DerivedJavaRoot], Unit](createProfile(acl), expr)
    assert(() === cexpr(SimpleContext(new DerivedJavaRoot)))
  }

  test("header test") {
    val acl = allow {
      new ju.ArrayList[String]
    }
    val expr = "new ArrayList[String]"
    val cexpr = compiler.getCompiledExpression[SimpleContext[Unit], ju.List[_]](createProfile(acl, "import java.util.ArrayList"), expr)
    assert(new ju.ArrayList[String] === cexpr(SimpleContext(())))
  }

  test("utils test") {
    val expr = "utilValue"
    val cexpr = compiler.getCompiledExpression[SimpleContext[Unit], Int](createProfile(Nil, "", "val utilValue = 42"), expr)
    assert(42 === cexpr(SimpleContext(())))
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
    val cexpr = compiler.getCompiledExpression[SimpleContext[Unit], String](createProfile(acl), expr)
    assert("42" === cexpr(SimpleContext(())))
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
      compiler.getCompiledExpression[SimpleContext[Unit], String](createProfile(acl), expr)
    }
  }

  test("static module member validation test") {
    val acl = allow {
      Some.apply _
    }
    val expr = "Some(42)"
    val cexpr = compiler.getCompiledExpression[SimpleContext[Unit], Option[Int]](createProfile(acl), expr)
    assert(Some(42) === cexpr(SimpleContext(())))
  }

  test("static module access validation test") {
    val acl = allow {
      Some.apply _
    }
    val expr = "Some"
    assertMemberAccessForbidden {
      compiler.getCompiledExpression[SimpleContext[Unit], Unit](createProfile(acl), expr)
    }
  }

  test("static module member validation test 2") {
    val acl = allow {
      Some.all.members
    }
    val expr = "Some.hashCode"
    assertMemberAccessForbidden {
      compiler.getCompiledExpression[SimpleContext[Unit], Unit](createProfile(acl), expr)
    }
  }

  test("explicit member-by-implicit validation test") {
    val acl = allow {
      on { s: String =>
        s.reverse
      }
    }
    val expr = "\"bippy\".reverse"
    val cexpr = compiler.getCompiledExpression[SimpleContext[Unit], String](createProfile(acl), expr)
    assert("yppib" === cexpr(SimpleContext(())))
  }

  test("plain implicit conversion validation test") {
    val acl = allow {
      Predef.augmentString _
      on { s: StringOps =>
        s.reverse
      }
    }
    acl foreach println
    val expr = "\"bippy\".reverse"
    val cexpr = compiler.getCompiledExpression[SimpleContext[Unit], String](createProfile(acl), expr)
    assert("yppib" === cexpr(SimpleContext(())))
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
      compiler.getCompiledExpression[SimpleContext[Unit], String](createProfile(acl), expr)
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
      compiler.getCompiledExpression[SimpleContext[Unit], String](createProfile(acl), expr)
    }
  }

  test("dynamic test") {
    val acl = allow {
      SomeDynamic.selectDynamic _
    }
    val expr = "com.avsystem.scex.compiler.SomeDynamic.dynamicProperty"
    val cexpr = compiler.getCompiledExpression[SimpleContext[Unit], String](createProfile(acl), expr)
    assert("dynamicProperty" === cexpr(SimpleContext(())))
  }
}
