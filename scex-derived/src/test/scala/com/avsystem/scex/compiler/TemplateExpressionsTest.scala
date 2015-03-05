package com.avsystem.scex
package compiler

import java.lang.annotation.RetentionPolicy
import java.{lang => jl, util => ju}

import com.avsystem.scex.compiler.ScexCompiler.CompilationFailedException
import com.avsystem.scex.util.{PredefinedAccessSpecs, SimpleContext}

import scala.runtime.BoxedUnit

/**
 * Created: 18-11-2013
 * Author: ghik
 */
class TemplateExpressionsTest extends ScexFunSuite with CompilationTest {

  import com.avsystem.scex.validation.SymbolValidator._

  test("literal string test") {
    assert("stuff" === evaluateTemplate[String]("stuff"))
  }

  test("literal string as Any test") {
    assert("stuff" === evaluateTemplate[Any]("stuff"))
  }

  test("single-argument template test") {
    assert("trololo 5 dafuq" === evaluateTemplate[String]("trololo ${15/3} dafuq"))
  }

  test("multiple-argument template test") {
    assert("trololo 5 dafuq 68" === evaluateTemplate[String]("trololo ${15/3} dafuq ${12+56}"))
  }

  test("single-argument int expression") {
    assert(5 === evaluateTemplate[Int]("${15/3}"))
  }

  test("single-argument int expression as Any test") {
    assert(5 === evaluateTemplate[Any]("${15/3}"))
  }

  test("single-argument null expression") {
    val nul: Any = null
    assert(nul === evaluateTemplate[Any]("${null}"))
    assert(nul === evaluateTemplate[AnyRef]("${null}"))
    assert(nul === evaluateTemplate[String]("${null}"))
  }

  test("null splicing test") {
    assert(" null" === evaluateTemplate[String](" ${null}"))
  }

  test("custom splicer test") {
    val acl = allow {
      on { fsr: FancySplicedRoot =>
        fsr.self
      }
      FancySplicedRoot.fancySplicer.toString(_: FancySplicedRoot)
    }
    val cexpr = compiler.getCompiledExpression[SimpleContext[FancySplicedRoot], String](
      createProfile(acl), "${self}stuff", template = true)
    assert("FANCYstuff" === cexpr.apply(SimpleContext(new FancySplicedRoot)))
  }

  test("single-argument int expression with non-blank surroundings") {
    intercept[CompilationFailedException] {
      evaluateTemplate[Int]("${15/3}srsly")
    }
  }

  test("multiple-argument int expression") {
    intercept[CompilationFailedException] {
      evaluateTemplate[Int]("${15/3} ${2342.60}")
    }
  }

  test("enum from string test") {
    assert(RetentionPolicy.RUNTIME === evaluateTemplate[RetentionPolicy]("${\"RUNTIME\"}", allow(RetentionPolicy.valueOf _)))
  }

  test("enum literally test") {
    assert(RetentionPolicy.RUNTIME === evaluateTemplate[RetentionPolicy]("${java.lang.annotation.RetentionPolicy.RUNTIME}"))
  }

  test("interpolation argument inference test") {
    val acl = PredefinedAccessSpecs.basicOperations ++ allow {
      on { vr: ValueRoot[_] =>
        vr.value
      }
    }
    val cexpr = compiler.getCompiledExpression[SimpleContext[ValueRoot[String]], String](
      createProfile(acl), "${if(value.endsWith(\"lol\")) value + \"lol\" else value}", template = true)

    assert("fuulollol" === cexpr(SimpleContext(new ValueRoot("fuulol"))))
  }

  test("multiple interpolation arguments inference test") {
    val acl = PredefinedAccessSpecs.basicOperations ++ allow {
      on { vr: ValueRoot[_] =>
        vr.value
      }
    }
    val cexpr = compiler.getCompiledExpression[SimpleContext[ValueRoot[String]], String](
      createProfile(acl), "${if(value.endsWith(\"lol\")) value + \"lol\" else value}${123}", template = true)

    assert("fuulollol123" === cexpr(SimpleContext(new ValueRoot("fuulol"))))
  }

  test("compilation errors merging test") {
    val exception = intercept[CompilationFailedException] {
      compiler.getCompiledExpression[SimpleContext[Unit], Any](createProfile(Nil), "fds${1+}asd${3+}abc")
    }
    exception.printStackTrace()
    assert(exception.errors.size === 2)
  }

  test("empty template to null coercion test - Any") {
    assert(evaluateTemplate[Any]("") === null)
  }

  test("empty template to null coercion test - String") {
    assert(evaluateTemplate[String]("") === null)
  }

  test("empty template to null coercion test - jl.Integer") {
    assert(evaluateTemplate[jl.Integer]("") === null)
  }

  test("empty template to null coercion test - Int") {
    intercept[CompilationFailedException] {
      evaluateTemplate[Int]("")
    }
  }

  test("empty block test") {
    val unit = ()
    assert(evaluateTemplate[Any]("${}") === unit)
    assert(evaluateTemplate[AnyRef]("${}") === "()")
    assert(evaluateTemplate[String]("${}") === "()")
  }
}
