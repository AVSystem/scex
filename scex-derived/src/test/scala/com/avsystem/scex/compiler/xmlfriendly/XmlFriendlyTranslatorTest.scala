package com.avsystem.scex
package compiler.xmlfriendly

import java.{util => ju, lang => jl}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

/**
 * Created: 23-04-2014
 * Author: ghik
 */
@RunWith(classOf[JUnitRunner])
class XmlFriendlyTranslatorTest extends FunSuite {

  import XmlFriendlyTranslator.translate

  test("variables test") {
    assert(" _vars.lol" === translate("#lol", template = false).result)
  }

  test("variables in template test") {
    assert("fuu #lol $lol haha ${ _vars.dafuq + 5} ss" ===
      translate("fuu #lol $lol haha ${#dafuq + 5} ss", template = true).result)
  }

  test("negated variable test") {
    assert("! _vars.costam" === translate("!#costam", template = false).result)
  }

  test("operator translation test") {
    assert("a || b &&  c" === translate("a or b and c", template = false).result)
  }

  test("operator translation in template test") {
    assert("a or ${b &&  c} or d" === translate("a or ${b and c} or d", template = true).result)
  }

  test("large template test") {
    val str = "lol" * 10000
    assert(str === translate(str, template = true).result)
  }

  test("large template test 2") {
    val str = "lol$$" * 10000
    assert(str === translate(str, template = true).result)
  }

}
