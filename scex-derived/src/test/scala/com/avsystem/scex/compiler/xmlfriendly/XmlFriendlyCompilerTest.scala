package com.avsystem.scex.compiler.xmlfriendly

import com.avsystem.scex.compiler.{SimpleContext, ScexCompilerConfig}
import com.avsystem.scex.japi.XmlFriendlyJavaScexCompiler
import com.avsystem.scex.validation.SymbolValidator._
import com.avsystem.scex.validation.{SymbolValidator, SyntaxValidator}
import com.avsystem.scex.{ExpressionProfile, PredefinedAccessSpecs}
import java.{util => ju, lang => jl}
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
 * Created: 17-09-2013
 * Author: ghik
 */
@RunWith(classOf[JUnitRunner])
class XmlFriendlyCompilerTest extends FunSuite {

  val compiler = new XmlFriendlyJavaScexCompiler(new ScexCompilerConfig)

  def createProfile(acl: List[MemberAccessSpec], header: String = "import com.avsystem.scex.compiler._", utils: String = "") =
    new ExpressionProfile(SyntaxValidator.SimpleExpressions, SymbolValidator(acl), header, utils)

  test("single quotes test") {
    val profile = createProfile(Nil)

    val expr = compiler.getCompiledExpression[SimpleContext[Unit], String](profile, "'single quoted string'")
    assert("single quoted string" === expr.apply(SimpleContext(())))
  }

  test("boolean expressions test") {
    val profile = createProfile(Nil)

    val expr = compiler.getCompiledExpression[SimpleContext[Unit], Boolean](profile, "true or true and false")
    assert(true === expr.apply(SimpleContext(())))
  }

  test("complex expression test") {
    val profile = createProfile(PredefinedAccessSpecs.basicOperations)
    val stringExpr = "letters`'\"lol`'{{\"} $$srsly and or ${'sqs'} ${true or {true; false}}"

    val expr = compiler.getCompiledExpression[SimpleContext[Unit], String](profile, stringExpr, template = true)
    assert("letters`'\"lol`'{{\"} $srsly and or sqs true" === expr.apply(SimpleContext(())))
  }

  test("dynamic variables test") {
    val acl = PredefinedAccessSpecs.basicOperations
    val expr = "#dafuq + 2345"
    val context = SimpleContext(())
    context.setVariable("dafuq", "srsly")
    val cexpr = compiler.getCompiledExpression[SimpleContext[Unit], String](createProfile(acl), expr)
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
}
