package com.avsystem.scex
package compiler

import com.avsystem.scex.PredefinedAccessSpecs
import com.avsystem.scex.compiler.ParameterizedClass.StaticInnerGeneric
import com.avsystem.scex.compiler.ScexCompiler.CompilationFailedException
import com.avsystem.scex.validation.SymbolValidator
import com.google.common.reflect.TypeToken
import java.{util => ju, lang => jl}
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import scala.collection.immutable.StringOps
import com.avsystem.scex.util.SimpleContext

@RunWith(classOf[JUnitRunner])
class ScexCompilerTest extends FunSuite with CompilationTest {

  import SymbolValidator._

  override def defaultAcl = Nil

  test("trivial compilation test") {
    assert(() === evaluate[Unit]("()"))
  }

  test("simple arithmetic expression") {
    assert(298 === evaluate[Int]("1+5+250+42"))
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
    val cexpr = compiler.buildExpression.contextType(new TypeToken[SimpleContext[RootType]] {}).template(false)
      .resultType(classOf[String]).profile(createProfile(acl)).expression(expr).get

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
    val cexpr = compiler.getCompiledExpression[SimpleContext[Unit], String](createProfile(acl), expr, template = false)
    assert("propertytruefalse" === cexpr(SimpleContext(())))
  }

  test("constructor allow test") {
    val acl = allow {
      new JavaRoot
    }
    val expr = "new JavaRoot"
    val cexpr = compiler.getCompiledExpression[SimpleContext[Unit], Unit](createProfile(acl), expr, template = false)
    assert(() === cexpr(SimpleContext(())))
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
    assert(() === cexpr(SimpleContext(new DerivedJavaRoot)))
  }

  test("header test") {
    val acl = allow {
      new ju.ArrayList[String]
    }
    val expr = "new ArrayList[String]"
    val cexpr = compiler.getCompiledExpression[SimpleContext[Unit], ju.List[_]](
      createProfile(acl, "import java.util.ArrayList"), expr, template = false)
    assert(new ju.ArrayList[String] === cexpr(SimpleContext(())))
  }

  test("utils test") {
    val expr = "utilValue"
    val cexpr = compiler.getCompiledExpression[SimpleContext[Unit], Int](
      createProfile(Nil, "", "val utilValue = 42"), expr, template = false)
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
    val cexpr = compiler.getCompiledExpression[SimpleContext[Unit], String](createProfile(acl), expr, template = false)
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
      compiler.getCompiledExpression[SimpleContext[Unit], String](createProfile(acl), expr, template = false)
    }
  }

  test("static module member validation test") {
    val acl = allow {
      Some.apply _
    }
    val expr = "Some(42)"
    val cexpr = compiler.getCompiledExpression[SimpleContext[Unit], Option[Int]](createProfile(acl), expr, template = false)
    assert(Some(42) === cexpr(SimpleContext(())))
  }

  test("static module access validation test") {
    val acl = allow {
      Some.apply _
    }
    val expr = "Some"
    assertMemberAccessForbidden {
      compiler.getCompiledExpression[SimpleContext[Unit], Unit](createProfile(acl), expr, template = false)
    }
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

  test("explicit member-by-implicit validation test") {
    val acl = allow {
      on { s: String =>
        s.reverse
      }
    }
    val expr = "\"bippy\".reverse"
    val cexpr = compiler.getCompiledExpression[SimpleContext[Unit], String](createProfile(acl), expr, template = false)
    assert("yppib" === cexpr(SimpleContext(())))
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

  test("dynamic test") {
    val acl = allow {
      SomeDynamic.selectDynamic _
    }
    val expr = "com.avsystem.scex.compiler.SomeDynamic.dynamicProperty"
    val cexpr = compiler.getCompiledExpression[SimpleContext[Unit], String](createProfile(acl), expr, template = false)
    assert("dynamicProperty" === cexpr(SimpleContext(())))
  }

}
