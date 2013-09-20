package com.avsystem.scex.compiler

import com.avsystem.scex.compiler.ParameterizedClass.StaticInnerGeneric
import com.avsystem.scex.compiler.ScexCompiler.CompilationFailedException
import com.avsystem.scex.validation.{SymbolValidator, SyntaxValidator}
import com.avsystem.scex.{Expression, TypeTag, PredefinedAccessSpecs}
import java.{util => ju, lang => jl}
import org.scalatest.FunSuite
import scala.collection.immutable.StringOps

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
    val cexpr = compiler.getCompiledExpression(createProfile(Nil), "()", classOf[Unit], classOf[Unit])
    assert(() === cexpr(()))
  }

  test("simple arithmetic expression") {
    val cexpr = compiler.getCompiledExpression(createProfile(Nil), "1+5+250+42", classOf[Unit], classOf[Int])
    assert(298 === cexpr(()))
  }

  test("strip dollar and brackets test") {
    val cexpr = compiler.getCompiledExpression(createProfile(Nil), "${1+5+250+42}", classOf[Unit], classOf[Int])
    assert(298 === cexpr(()))
  }

  test("string expression test") {
    val expr = "bippy \"${42/7}\" rest"
    val cexpr = compiler.getCompiledStringExpression(createProfile(PredefinedAccessSpecs.basicOperations), expr, classOf[Unit])
    assert("bippy \"6\" rest" === cexpr(()))
  }

  test("simple syntax validation test") {
    intercept[CompilationFailedException] {
      compiler.getCompiledExpression(createProfile(Nil), "while(true) {}", classOf[Unit], classOf[Unit])
    }
  }

  test("simple member validation test") {
    assertMemberAccessForbidden {
      compiler.getCompiledExpression(createProfile(Nil), "System.exit(0)", classOf[Unit], classOf[Unit])
    }
  }

  test("syntax error test") {
    intercept[CompilationFailedException] {
      compiler.getCompiledExpression(createProfile(Nil), "this doesn't make sense", classOf[Unit], classOf[Unit])
    }
  }

  test("context access test with java getter adapters") {
    val acl = PredefinedAccessSpecs.basicOperations ++ allow {
      on { jc: JavaContext =>
        jc.all.members
      }
    }
    val expr = "property + extraordinary + extraordinarilyBoxed + field + twice(42)"
    val cexpr = compiler.getCompiledExpression(createProfile(acl), expr, classOf[JavaContext], classOf[String])
    assert("propertytruefalse42.4284" === cexpr(new JavaContext))
  }

  test("complicated context type test with java getter adapters") {
    val acl = PredefinedAccessSpecs.basicOperations ++ allow {
      on { m: ju.Map[_, _] =>
        m.toString
      }
      on { ct: (ParameterizedClass.StaticInnerGeneric[A]#DeeplyInnerGeneric[B] forSome {type A; type B}) =>
        ct.all.members
      }
    }
    val expr = """ "EXPR:" + awesomeness + sampleMap + handleStuff("interesting stuff") + awesome + fjeld """
    val contextTypeTag = new TypeTag[ParameterizedClass.StaticInnerGeneric[Cloneable]#DeeplyInnerGeneric[_]] {}
    val cexpr = compiler.getCompiledExpression(createProfile(acl), expr, contextTypeTag, classOf[String])
    val sig = new StaticInnerGeneric[Cloneable]
    assert("EXPR:true{}[interesting stuff handled]true[fjeld]" === cexpr(new sig.DeeplyInnerGeneric[String]))
  }

  test("non-context java getter adapters test") {
    val acl = PredefinedAccessSpecs.basicOperations ++ allow {
      on { jc: JavaContext =>
        new JavaContext
        jc.all.members
      }
    }
    val expr = "new JavaContext().property + new JavaContext().extraordinary + new JavaContext().extraordinarilyBoxed"
    val cexpr = compiler.getCompiledExpression(createProfile(acl), expr, classOf[Unit], classOf[String])
    assert("propertytruefalse" === cexpr(()))
  }

  test("constructor allow test") {
    val acl = allow {
      new JavaContext
    }
    val expr = "new JavaContext"
    val cexpr = compiler.getCompiledExpression(createProfile(acl), expr, classOf[Unit], classOf[Unit])
    assert(() === cexpr(()))
  }

  test("constructor deny test") {
    val acl = deny {
      new JavaContext
    }
    val expr = "new JavaContext"
    assertMemberAccessForbidden {
      compiler.getCompiledExpression(createProfile(acl), expr, classOf[Unit], classOf[Unit])
    }
  }

  test("constructor not allow test") {
    val expr = "new JavaContext"
    assertMemberAccessForbidden {
      compiler.getCompiledExpression(createProfile(Nil), expr, classOf[Unit], classOf[Unit])
    }
  }

  test("java getter validation test") {
    val acl = allow {
      on { jc: JavaContext =>
        jc.getProperty
      }
    }
    val expr = "property"
    val cexpr = compiler.getCompiledExpression(createProfile(acl), expr, classOf[JavaContext], classOf[String])
    assert("property" === cexpr(new JavaContext))
  }

  test("validation test with subtyping") {
    val acl = deny {
      on { jc: JavaContext =>
        jc.getProperty
      }
    } ++ allow {
      on { djc: DerivedJavaContext =>
        djc.all.members
      }
    }
    val expr = "property"
    assertMemberAccessForbidden {
      compiler.getCompiledExpression(createProfile(acl), expr, classOf[DerivedJavaContext], classOf[String])
    }
  }

  test("validation test with overriding") {
    val acl = allow {
      on { jc: JavaContext =>
        jc.overriddenMethod()
      }
    }
    val expr = "overriddenMethod()"
    val cexpr = compiler.getCompiledExpression(createProfile(acl), expr, classOf[DerivedJavaContext], classOf[Unit])
    assert(() === cexpr(new DerivedJavaContext))
  }

  test("header test") {
    val acl = allow {
      new ju.ArrayList[String]
    }
    val expr = "new ArrayList[String]"
    val cexpr = compiler.getCompiledExpression(createProfile(acl, "import java.util.ArrayList"),
      expr, classOf[Unit], classOf[ju.List[_]])
    assert(new ju.ArrayList[String] === cexpr(()))
  }

  test("utils test") {
    val expr = "utilValue"
    val cexpr = compiler.getCompiledExpression(createProfile(Nil, "", "val utilValue = 42"),
      expr, classOf[Unit], classOf[Int])
    assert(42 === cexpr(()))
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
    val cexpr = compiler.getCompiledExpression(createProfile(acl), expr, classOf[Unit], classOf[String])
    assert("42" === cexpr(()))
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
      compiler.getCompiledExpression(createProfile(acl), expr, classOf[Unit], classOf[String])
    }
  }

  test("static module member validation test") {
    val acl = allow {
      Some.apply _
    }
    val expr = "Some(42)"
    val cexpr = compiler.getCompiledExpression(createProfile(acl), expr, classOf[Unit], classOf[Option[_]])
    assert(Some(42) === cexpr(()))
  }

  test("static module access validation test") {
    val acl = allow {
      Some.apply _
    }
    val expr = "Some"
    assertMemberAccessForbidden {
      compiler.getCompiledExpression(createProfile(acl), expr, classOf[Unit], classOf[Unit])
    }
  }

  test("static module member validation test 2") {
    val acl = allow {
      Some.all.members
    }
    val expr = "Some.hashCode"
    assertMemberAccessForbidden {
      compiler.getCompiledExpression(createProfile(acl), expr, classOf[Unit], classOf[Unit])
    }
  }

  test("explicit member-by-implicit validation test") {
    val acl = allow {
      on { s: String =>
        s.reverse
      }
    }
    val expr = "\"bippy\".reverse"
    val cexpr = compiler.getCompiledExpression(createProfile(acl), expr, classOf[Unit], classOf[String])
    assert("yppib" === cexpr(()))
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
    val cexpr = compiler.getCompiledExpression(createProfile(acl), expr, classOf[Unit], classOf[String])
    assert("yppib" === cexpr(()))
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
      compiler.getCompiledExpression(createProfile(acl), expr, classOf[Unit], classOf[String])
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
      compiler.getCompiledExpression(createProfile(acl), expr, classOf[Unit], classOf[String])
    }
  }

  test("dynamic test") {
    val acl = allow {
      SomeDynamic.selectDynamic _
    }
    val expr = "com.avsystem.scex.compiler.SomeDynamic.dynamicProperty"
    val cexpr: Expression[Unit, String] = compiler.getCompiledExpression(createProfile(acl), expr, classOf[Unit], classOf[String])
    assert("dynamicProperty" === cexpr(()))
  }
}
