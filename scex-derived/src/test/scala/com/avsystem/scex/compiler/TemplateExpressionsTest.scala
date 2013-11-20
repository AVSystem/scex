package com.avsystem.scex.compiler

import com.avsystem.scex.PredefinedAccessSpecs
import com.avsystem.scex.compiler.ScexCompiler.CompilationFailedException
import com.avsystem.scex.validation.SymbolValidator
import java.lang.annotation.RetentionPolicy
import java.{util => ju, lang => jl}
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import scala.reflect.runtime.universe.TypeTag

/**
 * Created: 18-11-2013
 * Author: ghik
 */
@RunWith(classOf[JUnitRunner])
class TemplateExpressionsTest extends FunSuite with CompilationTest {

  import SymbolValidator._

  test("string literal test") {
    assert("trololo dafuq" === evaluateTemplate[String]("trololo dafuq"))
  }

  test("string literal with fancy characters test") {
    assert( raw"""trololo "" \" \\ '' dafuq\n""" === evaluateTemplate[String]( raw"""trololo "" \" \\ '' dafuq\n"""))
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

  test("single-argument int expression with blank surroundings") {
    assert(5 === evaluateTemplate[Int]("  ${15/3}    "))
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

  test("boolean literal test") {
    assert(true === evaluateTemplate[Boolean]("true"))
  }

  test("boolean literal test with surrounding whitespaces") {
    assert(true === evaluateTemplate[Boolean](" true   "))
  }

  test("invalid boolean literal test") {
    intercept[CompilationFailedException] {
      evaluateTemplate[Boolean]("hueheuahueh")
    }
  }

  test("invalid boolean literal test - expression as literal") {
    intercept[CompilationFailedException] {
      evaluateTemplate[Boolean]("true && false")
    }
  }

  test("enum literal test") {
    assert(RetentionPolicy.RUNTIME === evaluateTemplate[RetentionPolicy]("  RUNTIME  "))
  }

  test("bad enum literal test") {
    intercept[CompilationFailedException] {
      evaluateTemplate[RetentionPolicy]("HUEHUE")
    }
  }

  test("enum from string test") {
    assert(RetentionPolicy.RUNTIME === evaluateTemplate[RetentionPolicy]("${\"RUNTIME\"}", allow(RetentionPolicy.valueOf _)))
  }

  test("enum literally test") {
    assert(RetentionPolicy.RUNTIME === evaluateTemplate[RetentionPolicy]("${java.lang.annotation.RetentionPolicy.RUNTIME}"))
  }
}
