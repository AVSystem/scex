package com.avsystem.scex.compiler

import java.{lang => jl, util => ju}

import com.avsystem.scex.parsing.TemplateParser

/**
 * Created: 03-11-2014
 * Author: ghik
 */
class TemplateParserTest extends ScexFunSuite {
  def parse(expr: String) = {
    val (parts, args) = TemplateParser.parseTemplate(expr).get
    (parts, args.map(_.result))
  }

  test("literal test") {
    assert(parse("") ===(List(""), Nil))
    assert(parse("stuff") ===(List("stuff"), Nil))
  }

  test("dollar test") {
    assert(parse("$$") ===(List("$"), Nil))
    assert(parse("abc$$def") ===(List("abc$def"), Nil))
  }

  test("single arg test") {
    assert(parse("$ident") ===(List("", ""), List("$ident")))
    assert(parse("${ident}") ===(List("", ""), List("${ident}")))
    assert(parse("abc${ident}") ===(List("abc", ""), List("${ident}")))
    assert(parse("${ident}def") ===(List("", "def"), List("${ident}")))
    assert(parse("abc${ident}def") ===(List("abc", "def"), List("${ident}")))
  }

  test("escaping test") {
    assert(parse("${\"}\"}") ===(List("", ""), List("${\"}\"}")))
  }

  test("nested blocks test") {
    assert(parse("${{{}{{}}{}}}}") ===(List("", "}"), List("${{{}{{}}{}}}")))
  }

}
