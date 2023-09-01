package com.avsystem.scex.compiler

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
    val acl = PredefinedAccessSpecs.basicOperations
    val expr = "#someDouble.toDegrees"
    val context = SimpleContext(())
    context.setTypedVariable("someDouble", math.Pi)

    val cexpr = compiler.buildExpression
      .contextType(ScalaTypeTokens.create[SimpleContext[Unit]])
      .resultType(classOf[Double])
      .expression(expr)
      .template(false)
      .variableClass("someDouble", classOf[Double])
      .profile(createProfile(acl))
      .get

    assert(180.0 == cexpr(context))
  }
}
