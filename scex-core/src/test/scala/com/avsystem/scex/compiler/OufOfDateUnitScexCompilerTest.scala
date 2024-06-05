package com.avsystem.scex.compiler

import com.avsystem.scex.compiler.presentation.ScexPresentationCompiler
import com.avsystem.scex.japi.{JavaScexCompiler, ScalaTypeTokens}
import com.avsystem.scex.util.{PredefinedAccessSpecs, SimpleContext}
import org.scalatest.funsuite.AnyFunSuite

class OufOfDateUnitScexCompilerTest extends AnyFunSuite with CompilationTest {

  override protected def createCompiler: noCacheCompiler.type = noCacheCompiler

  object noCacheCompiler
    extends ScexCompiler
      with ScexPresentationCompiler
      with JavaScexCompiler
      with ClassfileReusingScexCompiler {

    val settings = new ScexSettings
    settings.classfileDirectory.value = "testClassfileCache"
  }

  /** *
   * Purpose of this test it to compile same expression using same profile multiple times with disabled caching to force execution of backgroundCompile method
   * backgroundCompile was infinitely executed with changes introduced by 2.13.13
   */
  test("out of date compilation") {
    val acl = PredefinedAccessSpecs.basicOperations
    val profile = createProfile(acl)

    def compileExpression(): Unit = {
      compiler.buildExpression
        .contextType(ScalaTypeTokens.create[SimpleContext[Unit]])
        .resultType(classOf[String])
        .expression(s""""value"""")
        .template(false)
        .profile(profile)
        .get
    }

    compileExpression()
    compileExpression()
  }


}
