package com.avsystem.scex
package compiler

import java.util.concurrent.Callable
import java.{lang => jl, util => ju}
import org.scalatest.funsuite.AnyFunSuite

/**
 * Created: 17-10-2013
 * Author: ghik
 */
class ArbitraryCompilationTest extends AnyFunSuite {
  val compiler = new DefaultScexCompiler(new ScexSettings)

  test("arbitrary source code compilation test") {
    val code =
      """
        |package com.avsystem.scex
        |package test
        |
        |class Stuff extends java.util.concurrent.Callable[String] {
        |  def call = "stuff"
        |}
        |
      """.stripMargin

    val clazz = compiler.compileClass(code, "com.avsystem.scex.test.Stuff")
    val callable = clazz.getConstructor().newInstance().asInstanceOf[Callable[String]]

    assert("stuff" == callable.call())
  }
}
