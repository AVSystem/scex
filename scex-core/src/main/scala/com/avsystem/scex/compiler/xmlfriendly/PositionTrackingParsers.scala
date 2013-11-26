package com.avsystem.scex
package compiler.xmlfriendly

import java.{util => ju, lang => jl}
import scala.util.parsing.combinator.RegexParsers

/**
 * Extensions for Scala parser combinators that allow to turn Parser[String] instances into Parser[PString] instances
 * that hold information about differences between original and transformed string that later allow to map cursor
 * positions between the two.
 *
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
