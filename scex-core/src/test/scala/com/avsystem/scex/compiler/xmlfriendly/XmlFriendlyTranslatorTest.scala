package com.avsystem.scex
package compiler.xmlfriendly

import com.google.common.io.ByteStreams
import org.scalatest.FunSuite

/**
  * Created: 23-04-2014
  * Author: ghik
  */
class XmlFriendlyTranslatorTest extends FunSuite {

  import com.avsystem.scex.compiler.xmlfriendly.XmlFriendlyTranslator.translate

  test("variables test") {
    val original = " #lol"
    val translated = "  _vars.lol"
    val pstr = translate(original)
    assert(translated == pstr.result)
    assert(original.indices.map(pstr.positionMapping.apply) ==
      Seq(0, 2, 8, 9, 10))
    assert(translated.indices.map(pstr.positionMapping.reverse.apply) ==
      Seq(0, 0, 1, 1, 1, 1, 1, 1, 2, 3, 4))
  }

  test("variables in template test") {
    assert("fuu #lol $$lol ${fuu} haha ${ _vars.dafuq + 5} ss" ==
      translate("fuu #lol $lol ${fuu} haha ${#dafuq + 5} ss", template = true).result)
  }

  test("negated variable test") {
    assert("! _vars.costam" == translate("!#costam").result)
  }

  test("operator translation test") {
    assert("a || b &&  c" == translate("a or b and c").result)
  }

  test("operator translation in template test") {
    assert("a or ${b &&  c} or d" == translate("a or ${b and c} or d", template = true).result)
  }

  test("large template test") {
    val str = "lol" * 10000
    assert(str == translate(str, template = true).result)
  }

  test("large template test 2") {
    val str = "lol${}" * 10000
    assert(str == translate(str, template = true).result)
  }

  test("backticked variable test") {
    assert("${ _vars.`type`}" == translate("${#`type`}").result)
  }

  test("keyword variable test") {
    assert("${ _vars.`type`}" == translate("${#type}").result)
  }

  test("unused scala keyword test") {
    val original = "${type}"
    val translated = "${`type`}"
    val pstr = translate(original)
    assert(translated == pstr.result)
    assert(original.indices.map(pstr.positionMapping.apply) ==
      Seq(0, 1, 3, 4, 5, 6, 8))
    assert(translated.indices.map(pstr.positionMapping.reverse.apply) ==
      Seq(0, 1, 2, 2, 3, 4, 5, 5, 6))
  }

  test("unused scala keyword selection test") {
    assert("${costam.`type`}" == translate("${costam.type}").result)
  }

  test("literal dollars and newlines test") {
    val res =
      """${""}a
        |a$$b
        |b$$$$cc""".stripMargin
    assert(res == translate("${''}a\na$b\nb$$cc", template = true).result)
  }
}
