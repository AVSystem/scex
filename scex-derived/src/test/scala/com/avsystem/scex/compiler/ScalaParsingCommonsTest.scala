package com.avsystem.scex.compiler

import java.{lang => jl, util => ju}

import com.avsystem.scex.parsing.ScalaParsingCommons

/**
 * Created: 31-10-2014
 * Author: ghik
 */
class ScalaParsingCommonsTest extends ScexFunSuite {

  import com.avsystem.scex.parsing.ScalaParsingCommons._

  def success(parser: Parser[String], input: String) =
    parseAll(parser, input) match {
      case Success(`input`, _) => true
      case _ => false
    }

  def failure(parser: Parser[String], input: String) =
    !success(parser, input)

  test("ident test") {
    assert(success(ident, "srsly"))
    assert(success(ident, "srsly_"))
    assert(success(ident, "srsly__"))
    assert(success(ident, "srsly_dafuq"))
    assert(success(ident, "srsly_+=:"))
    assert(success(ident, "srsly_dafuq_+=:"))
    assert(failure(ident, "+=:"))
    assert(failure(ident, "srsly+=:"))
    assert(failure(ident, "0srsly"))
  }
}
