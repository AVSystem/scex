package com.avsystem.scex.compiler

import com.avsystem.scex.Expression
import com.avsystem.scex.compiler.CodeGeneration.{TypedVariables, escapedChar}
import com.avsystem.scex.compiler.JavaScexCompilerTest.SuspiciousCharsAllowedInVariableNames
import com.avsystem.scex.compiler.ScexCompiler.CompilationFailedException
import com.avsystem.scex.japi.{ScalaTypeTokens, XmlFriendlyJavaScexCompiler}
import com.avsystem.scex.util.{PredefinedAccessSpecs, SimpleContext}
import com.avsystem.scex.validation.SymbolValidator.MemberAccessSpec
import org.scalatest.funsuite.AnyFunSuite

/**
 * Author: ghik
 * Created: 26/10/15.
 */
class JavaScexCompilerTest extends AnyFunSuite with CompilationTest {
  override protected def createCompiler = new XmlFriendlyJavaScexCompiler(new ScexSettings)

  test("typed variables test") {
    // given a valid type variable & an expression that uses the typed variable
    val variableName = "someDouble"
    val expr = s"#$variableName.toDegrees"

    // then expression is compiled without errors
    val compiledExpr = compileExpression(expr, variableName)

    // when expression is evaluated with valid value in context
    val context = initContextWithTypedVariable(variableName, math.Pi)
    val result = compiledExpr(context)

    // then the result is correct
    assert(math.Pi.toDegrees == result)
  }

  TypedVariables.IllegalCharsInVarName.foreach { char =>
    test(s"typed variables fail if char [code=${char.toInt}, escaped=${escapedChar(char)}] is in var name test") {
      // given a typed variable with illegal char in name & an expression that uses the typed variable
      val variableName = s"2varName$char"
      val expr = s"#`$variableName`.toDegrees"

      // then compilation should fail
      withClue(s"[code=${char.toInt}] character in variable name [$variableName] should not be allowed") {
        assertThrows[CompilationFailedException] {
          compileExpression(expr, variableName)
        }
      }
    }
  }

  SuspiciousCharsAllowedInVariableNames.foreach { char =>
    test(s"typed variables work if char [code=$char.toInt] is in var name test") {
      // given a typed variable with valid name & an expression that uses the typed variable
      val variableName = s"""2varName$char"""
      val expr = s"#`$variableName`.toDegrees"

      // then expression is compiled without errors
      val compiledExpr = compileExpression(expr, variableName)

      // when expression is evaluated with valid value in context
      val context = initContextWithTypedVariable(variableName, math.Pi)
      val result = compiledExpr(context)

      // then the result is correct
      assert(math.Pi.toDegrees == result)
    }
  }

  private def initContextWithTypedVariable(
                                            variableName: String,
                                            value: Double
                                          ): SimpleContext[Unit] = {
    val ctx = SimpleContext(())
    ctx.setTypedVariable(variableName, value)
    ctx
  }

  private def compileExpression(
                                 expr: String,
                                 variableName: String,
                                 acl: List[MemberAccessSpec] = PredefinedAccessSpecs.basicOperations
                               ): Expression[SimpleContext[Unit], Double] =
    compiler.buildExpression
      .contextType(ScalaTypeTokens.create[SimpleContext[Unit]])
      .resultType(classOf[Double])
      .expression(expr)
      .template(false)
      .variableClass(variableName, classOf[Double])
      .profile(createProfile(acl))
      .get
}

object JavaScexCompilerTest {
  // These characters caused compilation errors if they were put in typed variable names until v1.35 release
  private val SuspiciousCharsAllowedInVariableNames: Seq[Char] = Seq(' ', '\t', '@', '/')
}
