package com.avsystem.scex
package compiler.xmlfriendly

import com.avsystem.commons.misc.TypeString
import com.avsystem.scex.compiler.ScexCompiler.{CompilationFailedException, CompileError}
import com.avsystem.scex.compiler.{ClassTaggedContext, CompilationTest, ScexSettings}
import com.avsystem.scex.japi.XmlFriendlyJavaScexCompiler
import com.avsystem.scex.util.{PredefinedAccessSpecs, SimpleContext}
import com.avsystem.scex.validation.SymbolValidator._
import org.scalatest.FunSuite

import scala.reflect.{ClassTag, classTag}

/**
 * Created: 17-09-2013
 * Author: ghik
 */
class XmlFriendlyCompilerTest extends FunSuite with CompilationTest {

  override protected def createCompiler = new XmlFriendlyJavaScexCompiler(new ScexSettings)

  test("single quotes test") {
    val profile = createProfile(Nil)

    val expr = compiler.getCompiledExpression[SimpleContext[Unit], String](profile, "'single quoted string'", template = false)
    assert("single quoted string" == expr.apply(SimpleContext(())))
  }

  test("boolean expressions test") {
    val profile = createProfile(Nil)

    val expr = compiler.getCompiledExpression[SimpleContext[Unit], Boolean](profile, "true or true and false", template = false)
    assert(expr.apply(SimpleContext(())))
  }

  test("complex expression test") {
    val profile = createProfile(PredefinedAccessSpecs.basicOperations)
    val stringExpr = "letters`'\"lol`'{{\"} $srsly and or ${'sqs'} ${true or {true; false}}"

    val expr = compiler.getCompiledExpression[SimpleContext[Unit], String](profile, stringExpr, template = true)
    assert("letters`'\"lol`'{{\"} $srsly and or sqs true" == expr.apply(SimpleContext(())))
  }

  test("disabled dynamic variables test") {
    val acl = PredefinedAccessSpecs.basicOperations
    val expr = "#dafuq + 2345"
    try {
      compiler.getCompiledExpression[SimpleContext[Unit], String](createProfile(acl, dynamicVariablesEnabled = false), expr, template = false)
    } catch {
      case CompilationFailedException(_, List(CompileError(source, column, msg))) =>
        assert(source == expr)
        assert(column == 1)
        assert(msg == "not found: value _vars")
    }
  }

  test("dynamic variables test") {
    val acl = PredefinedAccessSpecs.basicOperations
    val expr = "#dafuq + 2345"
    val context = SimpleContext(())
    context.setVariable("dafuq", "srsly")
    val cexpr = compiler.getCompiledExpression[SimpleContext[Unit], String](createProfile(acl), expr, template = false)
    assert("srsly2345" == cexpr(context))
  }

  test("typed variables test") {
    val acl = PredefinedAccessSpecs.basicOperations
    val expr = "#someDouble.toDegrees"
    val context = SimpleContext(())
    context.setTypedVariable("someDouble", math.Pi)

    def cexpr(dynamicVariablesEnabled: Boolean) = compiler.getCompiledExpression[SimpleContext[Unit], Double](
      profile = createProfile(acl, dynamicVariablesEnabled = dynamicVariablesEnabled),
      expression = expr,
      template = false,
      variableTypes = Map("someDouble" -> TypeString[Double])
    )

    assert(180.0 == cexpr(dynamicVariablesEnabled = false)(context))
    assert(180.0 == cexpr(dynamicVariablesEnabled = true)(context))
  }

  test("typed and dynamic variables test") {
    val acl = PredefinedAccessSpecs.basicOperations
    val expr = "#someDouble.toDegrees.toInt.toString + #angleUnit + ' angle'"
    val context = SimpleContext(())
    context.setTypedVariable("someDouble", math.Pi)
    context.setVariable("angleUnit", "deg")

    def cexpr(dynamicVariablesEnabled: Boolean) = compiler.getCompiledExpression[SimpleContext[Unit], String](
      profile = createProfile(acl, dynamicVariablesEnabled = dynamicVariablesEnabled),
      expression = expr,
      template = false,
      variableTypes = Map("someDouble" -> TypeString[Double])
    )

    try {
      cexpr(dynamicVariablesEnabled = false)(context)
    } catch {
      case CompilationFailedException(_, List(CompileError(source, _, msg))) =>
        assert(source == expr)
        assert(msg.startsWith("value angleUnit is not a member of _variableAccessor"))
    }
    assert("180deg angle" == cexpr(dynamicVariablesEnabled = true)(context))
  }

  test("tagged typed variables test") {
    val acl = PredefinedAccessSpecs.basicOperations ++ allow {
      ClassTag.Double
    }
    val expr = "#someDouble.toDegrees"
    val context = new ClassTaggedContext
    context.setTypedVariable("someDouble", math.Pi)(classTag[Double])
    val cexpr = compiler.getCompiledExpression[ClassTaggedContext, Double](createProfile(acl), expr, template = false,
      variableTypes = Map("someDouble" -> TypeString[Double]))
    assert(180.0 == cexpr(context))
  }

  test("forbidden tag for typed variables test") {
    val acl = PredefinedAccessSpecs.basicOperations
    val expr = "#someDouble.toDegrees"
    assertMemberAccessForbidden {
      compiler.getCompiledExpression[ClassTaggedContext, Double](createProfile(acl), expr, template = false,
        variableTypes = Map("someDouble" -> TypeString[Double]))
    }
  }

  test("dynamic variables interpolation test") {
    val acl = PredefinedAccessSpecs.basicOperations
    val expr = "lol ${#dafuq} wtf"
    val context = SimpleContext(())
    context.setVariable("dafuq", "srsly")
    val cexpr = compiler.getCompiledExpression[SimpleContext[Unit], String](createProfile(acl), expr, template = true)
    assert("lol srsly wtf" == cexpr(context))
  }

  test("dynamic variables unary operator test") {
    val acl = PredefinedAccessSpecs.basicOperations
    val expr = "!#shiet.toBoolean"
    val context = SimpleContext(())
    context.setVariable("shiet", "true")
    val cexpr = compiler.getCompiledExpression[SimpleContext[Unit], Boolean](createProfile(acl), expr, template = false)
    assert(!cexpr(context))
  }

  test("compilation error translation test") {
    val acl = PredefinedAccessSpecs.basicOperations
    val expr = "#variable fuu #other"
    try {
      compiler.getCompiledExpression[SimpleContext[Unit], String](createProfile(acl), expr, template = false)
    } catch {
      case CompilationFailedException(_, List(CompileError(source, column, _))) =>
        assert(source == expr)
        assert(column == 11)
    }
  }

  test("compilation error message test") {
    val expr = "abc${5 + #shiet.toLol}fuu"
    intercept[CompilationFailedException] {
      compiler.getCompiledExpression[SimpleContext[Unit], String](createProfile(Nil), expr)
    }.errors match {
      case List(CompileError("${5 + #shiet.toLol}", 14, _)) =>
    }

  }

  test("spliced argument validation test") {
    val header = "import com.avsystem.scex.util.TypesafeEquals._"
    assert("false" == evaluateTemplate[String]("${if(4 > 5) true else false}", header = header))
  }
}
