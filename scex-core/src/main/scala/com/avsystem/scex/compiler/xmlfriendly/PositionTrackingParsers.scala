package com.avsystem.scex.compiler.xmlfriendly

import java.{util => ju, lang => jl}
import scala.util.parsing.combinator.RegexParsers

/**
 * Created: 21-10-2013
 * Author: ghik
 */
trait PositionTrackingParsers extends RegexParsers {

  implicit class stringPlusPString(str: String) {
    def ~+(pstr: PString): PString =
      if (str.nonEmpty)
        pstr match {
          case PString(result, offset, length, mods) =>
            PString(str + result, offset, length, Modification(offset, str.length) +: mods)
        }
      else pstr
  }

  class ParserWithPos(parser: Parser[String]) extends Parser[PString] {
    def apply(in: Input) = parser(in).map { str =>
      PString(str, in.offset, in.offset + str.length, Vector.empty)
    }
  }

  implicit class stringWithPos(pattern: String) {
    def p: Parser[PString] =
      new ParserWithPos(pattern)

    def rp: Parser[PString] =
      new ParserWithPos(pattern.r)
  }

  def join(pstrs: Traversable[PString]) =
    pstrs.reduce(_ + _)

}
