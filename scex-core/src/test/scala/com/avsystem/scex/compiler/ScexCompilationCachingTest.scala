package com.avsystem.scex.compiler

import com.avsystem.scex.ExpressionProfile
import com.avsystem.scex.compiler.ScexCompiler.CompileError
import com.avsystem.scex.japi.{DefaultJavaScexCompiler, JavaScexCompiler, ScalaTypeTokens}
import com.avsystem.scex.util.{PredefinedAccessSpecs, SimpleContext}
import com.google.common.util.concurrent.UncheckedExecutionException
import org.scalatest.funsuite.AnyFunSuite

final class ScexCompilationCachingTest extends AnyFunSuite with CompilationTest {

  private var compilationCount = 0

  private val settings = new ScexSettings
  settings.classfileDirectory.value = "testClassfileCache"
  settings.noGetterAdapters.value = true // to reduce number of compilations in tests

  private val acl = PredefinedAccessSpecs.basicOperations
  private val defaultProfile = createProfile(acl, utils = "val utilValue = 42")

  private def createFailingCompiler: JavaScexCompiler =
    new DefaultJavaScexCompiler(settings) {
      override protected def compile(sourceFile: ScexSourceFile): Either[ScexClassLoader, List[CompileError]] = {
        compilationCount += 1
        if (compilationCount == 1) throw new NullPointerException()
        else super.compile(sourceFile)
      }
    }

  private def createCountingCompiler: JavaScexCompiler =
    new DefaultJavaScexCompiler(settings) {
      override protected def compile(sourceFile: ScexSourceFile): Either[ScexClassLoader, List[CompileError]] = {
        compilationCount += 1
        super.compile(sourceFile)
      }
    }

  override def newProfileName(): String = "constant_name"

  private def compileExpression(
    compiler: JavaScexCompiler,
    expression: String = s""""value"""",
    profile: ExpressionProfile = defaultProfile,
  ): Unit = {
    compiler.buildExpression
      .contextType(ScalaTypeTokens.create[SimpleContext[Unit]])
      .resultType(classOf[String])
      .expression(expression)
      .template(false)
      .profile(profile)
      .get
  }

  test("Unexpected exceptions should be cached by default") {
    compilationCount = 0
    val compiler = createFailingCompiler

    assertThrows[UncheckedExecutionException](compileExpression(compiler))
    assert(compilationCount == 1)
    assertThrows[UncheckedExecutionException](compileExpression(compiler))
    assert(compilationCount == 1) // result fetched from cache
  }

  test("Unexpected exceptions shouldn't be cached when disabled using ScexSettings") {
    compilationCount = 0
    val compiler = createFailingCompiler
    compiler.settings.cacheUnexpectedCompilationExceptions.value = false

    assertThrows[UncheckedExecutionException](compileExpression(compiler))
    assert(compilationCount == 1) // utils compilation ended with NPE
    compileExpression(compiler)
    assert(compilationCount == 3) // 2x utils compilation + 1x final expression compilation
  }

  test("CompilationFailedExceptions should always be cached") {
    compilationCount = 0
    val compiler = createCountingCompiler
    val profile = createProfile(acl, utils = """invalidValue""")

    compiler.settings.cacheUnexpectedCompilationExceptions.value = true
    assertThrows[UncheckedExecutionException](compileExpression(compiler, profile = profile))
    assert(compilationCount == 1)
    assertThrows[UncheckedExecutionException](compileExpression(compiler, profile = profile))
    assert(compilationCount == 1)

    compiler.settings.cacheUnexpectedCompilationExceptions.value = false
    assertThrows[UncheckedExecutionException](compileExpression(compiler, profile = profile))
    assert(compilationCount == 1)
    assertThrows[UncheckedExecutionException](compileExpression(compiler, profile = profile))
    assert(compilationCount == 1)
  }

  test("Successful compilation should always be cached") {
    compilationCount = 0
    val compiler = createCountingCompiler

    compiler.settings.cacheUnexpectedCompilationExceptions.value = true
    compileExpression(compiler)
    assert(compilationCount == 2) // utils + expression value
    compileExpression(compiler)
    assert(compilationCount == 2)

    compiler.settings.cacheUnexpectedCompilationExceptions.value = false
    compileExpression(compiler)
    assert(compilationCount == 2)
    compileExpression(compiler)
    assert(compilationCount == 2)
  }
}
