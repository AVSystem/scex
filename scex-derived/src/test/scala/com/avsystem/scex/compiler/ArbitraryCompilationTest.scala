package com.avsystem.scex.compiler

import java.util.concurrent.Callable
import java.{util => ju, lang => jl}
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
 * Created: 17-10-2013
 * Author: ghik
 */
@RunWith(classOf[JUnitRunner])
class ArbitraryCompilationTest extends FunSuite {
  val compiler = new DefaultScexCompiler(new ScexCompilerConfig)

  test("arbitrary source code compilation test") {
    val code =
      """
        |package com.avsystem.scex.test
        |
        |class Stuff extends java.util.concurrent.Callable[String] {
        |  def call = "stuff"
        |}
        |
      """.stripMargin

    val clazz = compiler.compileClass(code, "com.avsystem.scex.test.Stuff")
    val callable = clazz.newInstance.asInstanceOf[Callable[String]]

    assert("stuff" === callable.call())
  }
}
