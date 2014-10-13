package com.avsystem.scex
package compiler

import com.avsystem.scex.compiler.ScexCompiler.CompilationFailedException
import com.avsystem.scex.validation.SymbolValidator
import java.lang.annotation.RetentionPolicy
import java.{util => ju, lang => jl}
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import com.avsystem.scex.util.SimpleContext

/**
 * Created: 18-11-2013
 * Author: ghik
 */
@RunWith(classOf[JUnitRunner])
class TemplateExpressionsTest extends FunSuite with CompilationTest {

  import SymbolValidator._

  test("literal string test") {
    assert("stuff" === evaluateTemplate[String]("stuff"))
  }

  test("literal string as Any test") {
    assert("stuff" === evaluateTemplate[Any]("stuff"))
  }

  test("single-argument string expression test") {
    assert("trololo 5 dafuq" === evaluateTemplate[String]("trololo ${15/3} dafuq"))
  }

  test("multiple-argument string expression test") {
    assert("trololo 5 dafuq 68" === evaluateTemplate[String]("trololo ${15/3} dafuq ${12+56}"))
  }

  test("single-argument int expression") {
    assert(5 === evaluateTemplate[Int]("${15/3}"))
  }

  test("single-argument int expression as Any test") {
    assert(5 === evaluateTemplate[Any]("${15/3}"))
  }

  test("custom splicer test") {
    val acl = allow {
      on { fsr: FancySplicedRoot =>
        fsr.self
      }
      FancySplicedRoot.fancySplicer.toString(_: FancySplicedRoot)
    }
    val cexpr = compiler.getCompiledExpression[SimpleContext[FancySplicedRoot], String](
      createProfile(acl), "${self}", template = true)
    assert("FANCY" === cexpr.apply(SimpleContext(new FancySplicedRoot)))
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
}
