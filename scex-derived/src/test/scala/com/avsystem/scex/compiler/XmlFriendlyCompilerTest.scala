package com.avsystem.scex.compiler

import com.avsystem.scex.PredefinedAccessSpecs
import com.avsystem.scex.validation.SymbolValidator._
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

    val expr = compiler.getCompiledExpression[Unit, String](profile, "'single quoted string'")
    assert("single quoted string" === expr.apply(()))
  }

  test("boolean expressions test") {
    val profile = createProfile(Nil)

    val expr = compiler.getCompiledExpression[Unit, Boolean](profile, "true or true and false")
    assert(true === expr.apply(()))
  }

  test("complex expression test") {
    val profile = createProfile(PredefinedAccessSpecs.basicOperations)
    val stringExpr = "letters`'\"lol`'{{\"} $$srsly and or ${'sqs'} ${true or {true; false}}"

    val expr = compiler.getCompiledStringExpression[Unit](profile, stringExpr)
    assert("letters`'\"lol`'{{\"} $srsly and or sqs true" === expr.apply(()))
  }

  test("dynamic variables test") {
    val acl = PredefinedAccessSpecs.basicOperations ++ allow {
      on { dv: DynamicVariables =>
        dv.all.members
      }
    }
    val expr = "$dafuq + 2345"
    val context = new DynamicVariables
    context.set("dafuq", "srsly")
    val cexpr = compiler.getCompiledExpression[DynamicVariables, String](createProfile(acl), expr)
    assert("srsly2345" === cexpr(context))
  }

  test("dynamic variables interpolation test") {
    val acl = PredefinedAccessSpecs.basicOperations ++ allow {
      on { dv: DynamicVariables =>
        dv.all.members
      }
    }
    val expr = "lol $dafuq wtf"
    val context = new DynamicVariables
    context.set("dafuq", "srsly")
    val cexpr = compiler.getCompiledStringExpression[DynamicVariables](createProfile(acl), expr)
    assert("lol srsly wtf" === cexpr(context))
  }
}
