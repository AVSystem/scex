package com.avsystem.scex
package compiler.xmlfriendly

import java.{lang => jl, util => ju}

import com.avsystem.scex.compiler.ScexCompiler.{CompileError, CompilationFailedException}
import com.avsystem.scex.compiler.{ScexSettings, ScexFunSuite}
import com.avsystem.scex.japi.XmlFriendlyJavaScexCompiler
import com.avsystem.scex.util.SimpleContext
import com.avsystem.scex.validation.SymbolValidator._
import com.avsystem.scex.validation.{SymbolValidator, SyntaxValidator}

/**
 * Created: 17-09-2013
 * Author: ghik
 */
class XmlFriendlyCompilerTest extends ScexFunSuite {

  val compiler = new XmlFriendlyJavaScexCompiler(new ScexSettings)

  def createProfile(acl: List[MemberAccessSpec], header: String = "import com.avsystem.scex.compiler._", utils: String = "") =
    new ExpressionProfile("test", SyntaxValidator.SimpleExpressions, SymbolValidator(acl), header, NamedSource("test", utils))

  test("single quotes test") {
    val profile = createProfile(Nil)

    val expr = compiler.getCompiledExpression[SimpleContext[Unit], String](profile, "'single quoted string'", template = false)
    assert("single quoted string" === expr.apply(SimpleContext(())))
  }

  test("boolean expressions test") {
    val profile = createProfile(Nil)

    val expr = compiler.getCompiledExpression[SimpleContext[Unit], Boolean](profile, "true or true and false", template = false)
    assert(true === expr.apply(SimpleContext(())))
  }

  test("complex expression test") {
    val profile = createProfile(PredefinedAccessSpecs.basicOperations)
    val stringExpr = "letters`'\"lol`'{{\"} $srsly and or ${'sqs'} ${true or {true; false}}"

    val expr = compiler.getCompiledExpression[SimpleContext[Unit], String](profile, stringExpr, template = true)
    assert("letters`'\"lol`'{{\"} $srsly and or sqs true" === expr.apply(SimpleContext(())))
  }

  test("dynamic variables test") {
    val acl = PredefinedAccessSpecs.basicOperations
    val expr = "#dafuq + 2345"
    val context = SimpleContext(())
    context.setVariable("dafuq", "srsly")
    val cexpr = compiler.getCompiledExpression[SimpleContext[Unit], String](createProfile(acl), expr, template = false)
    assert("srsly2345" === cexpr(context))
  }

  test("dynamic variables interpolation test") {
    val acl = PredefinedAccessSpecs.basicOperations
    val expr = "lol ${#dafuq} wtf"
    val context = SimpleContext(())
    context.setVariable("dafuq", "srsly")
    val cexpr = compiler.getCompiledExpression[SimpleContext[Unit], String](createProfile(acl), expr, template = true)
    assert("lol srsly wtf" === cexpr(context))
  }

  test("dynamic variables unary operator test") {
    val acl = PredefinedAccessSpecs.basicOperations
    val expr = "!#shiet.toBoolean"
    val context = SimpleContext(())
    context.setVariable("shiet", "true")
    val cexpr = compiler.getCompiledExpression[SimpleContext[Unit], Boolean](createProfile(acl), expr, template = false)
    assert(false === cexpr(context))
  }

  test("compilation error translation test") {
    val acl = PredefinedAccessSpecs.basicOperations
    val expr = "#variable fuu #other"
    try {
      compiler.getCompiledExpression[SimpleContext[Unit], String](createProfile(acl), expr, template = false)
    } catch {
      case CompilationFailedException(_, List(CompileError(source, column, _))) =>
        assert(source === expr)
        assert(column === 11)
    }
  }

  test("evaluation exception translation test") {
    val acl = PredefinedAccessSpecs.basicOperations
    val expr = "#nonexistent.toString"
    val cexpr = compiler.getCompiledExpression[SimpleContext[Unit], String](createProfile(acl), expr, template = false)
    try cexpr(SimpleContext(())) catch {
      case ee: EvaluationException =>
        assert(ee.getMessage === new EvaluationException(expr, 1, null).getMessage)
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
}
