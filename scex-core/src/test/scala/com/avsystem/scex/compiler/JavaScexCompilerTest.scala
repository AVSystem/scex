package com.avsystem.scex.compiler

import com.avsystem.scex.compiler.CodeGeneration.{TypedVariables, escapedChar}
import com.avsystem.scex.compiler.JavaScexCompilerTest.SuspiciousCharsAllowedInVariableNames
import com.avsystem.scex.compiler.ScexCompiler.CompilationFailedException
import com.avsystem.scex.japi.{ScalaTypeTokens, XmlFriendlyJavaScexCompiler}
import com.avsystem.scex.util.{PredefinedAccessSpecs, SimpleContext}
import org.scalatest.funsuite.AnyFunSuite

/**
 * Author: ghik
 * Created: 26/10/15.
 */
class JavaScexCompilerTest extends AnyFunSuite with CompilationTest {
  override protected def createCompiler = new XmlFriendlyJavaScexCompiler(new ScexSettings)

  test("typed variables test") {
    // given a context with a valid typed variable
    val acl = PredefinedAccessSpecs.basicOperations
    val context = SimpleContext(())
    context.setTypedVariable("someDouble", math.Pi)

    // given expression that uses the typed variable
    val expr = "#someDouble.toDegrees"

    // then expression is compiled without errors
    val cexpr = compiler.buildExpression
      .contextType(ScalaTypeTokens.create[SimpleContext[Unit]])
      .resultType(classOf[Double])
      .expression(expr)
      .template(false)
      .variableClass("someDouble", classOf[Double])
      .profile(createProfile(acl))
      .get

    // when expression is evaluated
    val result = cexpr(context)

    // then the result is correct
    assert(math.Pi.toDegrees == result)
  }

  TypedVariables.IllegalCharsInVarName.foreach { char =>
    test(s"typed variables fail if char [code=${char.toInt}, escaped=${escapedChar(char)}] is in var name test") {
      // given a context with a typed variable with illegal char in name
      val acl = PredefinedAccessSpecs.basicOperations
      val variableName = s"JP2NadajePoRazPierwszy$char"
      val context = SimpleContext(())
      context.setTypedVariable(variableName, math.Pi)

      // given expression that uses the typed variable
      val expr = s"#`$variableName`.toDegrees"

      // then compilation should fail
      withClue(s"[code=${char.toInt}] character in variable name [$variableName] should not be allowed") {
        assertThrows[CompilationFailedException] {
          compiler.buildExpression
            .contextType(ScalaTypeTokens.create[SimpleContext[Unit]])
            .resultType(classOf[Double])
            .expression(expr)
            .template(false)
            .variableClass(variableName, classOf[Double])
            .profile(createProfile(acl))
            .get
        }
      }
    }
  }

  SuspiciousCharsAllowedInVariableNames.foreach { char =>
    test(s"typed variables work if char [code=$char.toInt] is in var name test") {
      // given a context with a typed variable with non-standard characters in name
      val acl = PredefinedAccessSpecs.basicOperations
      val variableName = s"""2JPZnowuNadaje$char"""
      val context = SimpleContext(())
      context.setTypedVariable(variableName, math.Pi)

      // given expression that uses the typed variable
      val expr = s"#`$variableName`.toDegrees"

      // then expression is compiled without errors
      val cexpr = compiler.buildExpression
        .contextType(ScalaTypeTokens.create[SimpleContext[Unit]])
        .resultType(classOf[Double])
        .expression(expr)
        .template(false)
        .variableClass(variableName, classOf[Double])
        .profile(createProfile(acl))
        .get

      // when expression is evaluated
      val result = cexpr(context)

      // then the result is correct
      assert(math.Pi.toDegrees == result)
    }
  }

}

object JavaScexCompilerTest {
  // These characters caused compilation errors if they were put in variable names until v1.35 release
  private val SuspiciousCharsAllowedInVariableNames: Seq[Char] = Seq(' ', '\t', '@', '/')
}
