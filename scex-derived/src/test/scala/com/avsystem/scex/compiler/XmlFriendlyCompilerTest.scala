package com.avsystem.scex.compiler

import com.avsystem.scex.PredefinedAccessSpecs
import com.avsystem.scex.validation.SymbolValidator.MemberAccessSpec
import com.avsystem.scex.validation.{SymbolValidator, SyntaxValidator}
import java.{util => ju, lang => jl}
import org.scalatest.FunSuite

/**
 * Created: 17-09-2013
 * Author: ghik
 */
class XmlFriendlyCompilerTest extends FunSuite {

  val compiler = new XmlFriendlyJavaScexCompiler(new ScexCompilerConfig)

  def createProfile(acl: List[MemberAccessSpec], header: String = "import com.avsystem.scex.compiler._", utils: String = "") =
    new ExpressionProfile(SyntaxValidator.SimpleExpressions, SymbolValidator(acl), header, utils)

  test("single quotes test") {
    val profile = createProfile(Nil)

    val expr = compiler.getCompiledExpression(profile, "'single quoted string'", classOf[Unit], classOf[String])
    assert("single quoted string" === expr.apply(()))
  }

  test("boolean expressions test") {
    val profile = createProfile(Nil)

    val expr = compiler.getCompiledExpression(profile, "true or true and false", classOf[Unit], classOf[Boolean])
    assert(true === expr.apply(()))
  }

  test("complex expression test") {
    val profile = createProfile(PredefinedAccessSpecs.basicOperations)
    val stringExpr = "letters`'\"lol`'{{\"} $$srsly and or ${'sqs'} ${true or {true; false}}"

    val expr = compiler.getCompiledStringExpression(profile, stringExpr, classOf[Unit])
    assert("letters`'\"lol`'{{\"} $srsly and or sqs true" === expr.apply(()))
  }

}
