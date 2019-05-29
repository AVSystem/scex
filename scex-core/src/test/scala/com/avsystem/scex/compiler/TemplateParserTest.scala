package com.avsystem.scex.compiler

import com.avsystem.scex.compiler.xmlfriendly.XmlFriendlyTranslator
import com.avsystem.scex.parsing.TemplateParser
import com.google.common.io.ByteStreams
import org.scalatest.FunSuite

/**
 * Created: 03-11-2014
 * Author: ghik
 */
class TemplateParserTest extends FunSuite {
  def parse(expr: String): (List[String], List[String]) = {
    val (parts, args) = TemplateParser.parseTemplate(expr).get
    (parts, args.map(_.result))
  }

  test("literal test") {
    assert(parse("") == (List(""), Nil))
    assert(parse("stuff") == (List("stuff"), Nil))
  }

  test("dollar test") {
    assert(parse("$$") == (List("$"), Nil))
    assert(parse("abc$$def") == (List("abc$def"), Nil))
  }

  test("single arg test") {
    assert(parse("$ident") == (List("", ""), List("$ident")))
    assert(parse("${ident}") == (List("", ""), List("${ident}")))
    assert(parse("abc${ident}") == (List("abc", ""), List("${ident}")))
    assert(parse("${ident}def") == (List("", "def"), List("${ident}")))
    assert(parse("abc${ident}def") == (List("abc", "def"), List("${ident}")))
  }

  test("escaping test") {
    assert(parse("${\"}\"}") == (List("", ""), List("${\"}\"}")))
  }

  test("nested blocks test") {
    assert(parse("${{{}{{}}{}}}}") == (List("", "}"), List("${{{}{{}}{}}}")))
  }

  test("dollars and newlines") {
    val tpl =
      """${""}a
        |a$$b
        |b${""}""".stripMargin

    assert(parse(tpl) == (List("", "a\na$b\nb", ""), List("""${""}""", """${""}""")))
  }
}
