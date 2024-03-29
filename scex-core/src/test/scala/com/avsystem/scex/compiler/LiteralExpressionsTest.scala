package com.avsystem.scex
package compiler

import java.lang.annotation.RetentionPolicy
import com.avsystem.scex.compiler.ScexCompiler.CompilationFailedException
import com.avsystem.scex.compiler.TestUtils.CustomBooleanConversionRoot
import com.avsystem.scex.util.{PredefinedAccessSpecs, SimpleContext}
import org.scalatest.funsuite.AnyFunSuite

/**
 * Created: 04-04-2014
 * Author: ghik
 */
class LiteralExpressionsTest extends AnyFunSuite with CompilationTest {

  import com.avsystem.scex.validation.SymbolValidator._

  test("string literal test") {
    assert("trololo dafuq" == evaluateTemplate[String]("trololo dafuq"))
  }

  test("string literal with money test") {
    assert("hajs$hajs" == evaluateTemplate[String]("hajs$$hajs"))
  }

  test("string literal with fancy characters test") {
    assert(raw"""trololo "" \" \\ '' dafuq\n""" == evaluateTemplate[String](raw"""trololo "" \" \\ '' dafuq\n"""))
  }

  test("boolean literal test") {
    assert(evaluateTemplate[Boolean]("true"))
  }

  test("boolean literal test with surrounding whitespaces") {
    intercept[CompilationFailedException] {
      evaluateTemplate[Boolean](" true   ")
    }
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
    assert(RetentionPolicy.RUNTIME == evaluateTemplate[RetentionPolicy]("RUNTIME"))
  }

  test("bad enum literal test 1") {
    intercept[CompilationFailedException] {
      evaluateTemplate[RetentionPolicy]("  RUNTIME  ")
    }
  }

  test("bad enum literal test 2") {
    intercept[CompilationFailedException] {
      evaluateTemplate[RetentionPolicy]("HUEHUE")
    }
  }

  test("no literal conversion found test") {
    intercept[CompilationFailedException] {
      evaluateTemplate[List[String]]("hueheuehuaheh")
    }
  }

  test("custom literal conversion test") {
    val acl = PredefinedAccessSpecs.basicOperations ++ allow {
      TestUtils.all.introduced.members
    }

    val header = "import com.avsystem.scex.compiler.TestUtils.zeroOneLiteralToBoolean"
    assert(true == evaluateTemplate[Boolean]("1", acl, header))
  }

  test("disallowed custom literal conversion test") {
    intercept[CompilationFailedException] {
      val header = "import com.avsystem.scex.compiler.TestUtils.zeroOneLiteralToBoolean"
      evaluateTemplate[Boolean]("1", header = header)
    }
  }

  test("input dependent custom literal conversion test") {
    val acl = PredefinedAccessSpecs.basicOperations ++ allow {
      on { r: CustomBooleanConversionRoot =>
        r.all.introduced.members
      }
    }

    val cexpr = compiler.getCompiledExpression[SimpleContext[CustomBooleanConversionRoot], Boolean](
      createProfile(acl), "TRÓ", template = true, header = "")

    assert(cexpr(SimpleContext(new CustomBooleanConversionRoot("ZUO", "TRÓ"))))
  }

  test("java inner enum test") {
    val acl = allow {
      on { ei: EnumInside =>
        ei.all.members
      }
    }
    assert(EnumInside.TheEnum.THIS == evaluateTemplate[EnumInside.TheEnum]("THIS", acl))
  }

}
