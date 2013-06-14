package com.avsystem.scex.compiler

import com.avsystem.scex.compiler.ScexCompiler.CompilationFailedException
import com.avsystem.scex.validation.{SymbolValidator, SyntaxValidator}
import java.{util => ju, lang => jl}
import org.scalatest.FunSuite
import com.avsystem.scex.Utils

class CompilationTest extends FunSuite {

  import SymbolValidator._

  val compiler = new ScexCompiler(new ScexCompilerConfig)

  def catchAndPrint(code: => Any): Unit = try code catch {
    case t: Throwable => t.printStackTrace(System.out)
  }

  def createProfile(acl: List[MemberAccessSpec]) =
    new ExpressionProfile(SyntaxValidator.SimpleExpressions, new SymbolValidator(acl), "", "")

  test("trivial test") {
    val cexpr = compiler.getCompiledExpression(createProfile(Nil), "()", classOf[Unit], classOf[Unit])
    expectResult(())(cexpr(()))
  }

  test("simple arithmetic expression") {
    val cexpr = compiler.getCompiledExpression(createProfile(Nil), "1+5+250+42", classOf[Unit], classOf[Int])
    expectResult(298)(cexpr(()))
  }

  test("simple syntax validation test") {
    intercept[CompilationFailedException] {
      compiler.getCompiledExpression(createProfile(Nil), "while(true) {}", classOf[Unit], classOf[Unit])
    }
  }

  test("simple member validation test") {
    intercept[CompilationFailedException] {
      compiler.getCompiledExpression(createProfile(Nil), "System.exit(0)", classOf[Unit], classOf[Unit])
    }
  }

  test("syntax error test") {
    intercept[CompilationFailedException] {
      compiler.getCompiledExpression(createProfile(Nil), "this doesn't make sense", classOf[Unit], classOf[Unit])
    }
  }

  test("simple context test with java getter adapter") {
    val acl = Utils.basicOperations ++ allow {
      on { jc: JavaContext =>
        jc.all.members
      }
    }
    val expr = "property + extraordinary + extraordinarilyBoxed + field + twice(42)"
    val cexpr = compiler.getCompiledExpression(createProfile(acl), expr, classOf[JavaContext], classOf[String])
    assert("propertytruefalse42.4284" === cexpr(new JavaContext))
  }
  test("non-trivial context and return types test") {}
  test("java getter adapters test") {}
}
