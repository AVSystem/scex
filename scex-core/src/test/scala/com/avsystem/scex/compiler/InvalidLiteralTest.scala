package com.avsystem.scex.compiler

import com.avsystem.scex.compiler.ScexCompiler.CompilationFailedException
import com.avsystem.scex.japi.{DefaultJavaScexCompiler, JavaScexCompiler}
import org.scalatest.funsuite.AnyFunSuite

class InvalidLiteralTest extends AnyFunSuite with CompilationTest {
  override protected def createCompiler: JavaScexCompiler = {
    val settings = new ScexSettings
    settings.classfileDirectory.value = "testClassfileCache"
    //evaluating getter adapters fixes the problem (since the invalid expression isn't the first thing compiled)
    settings.noGetterAdapters.value = true
    new DefaultJavaScexCompiler(settings)
  }

  test("Invalid literal should not break the scex compiler") {
    intercept[CompilationFailedException] {
      evaluate[String]("11compilation_fail11")
    }
    evaluate[Unit]("()")
  }
}
