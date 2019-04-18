package com.avsystem.scex.parsing

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

  class ParserWithPos(parser: Parser[String]) extends Parser[PString] {
    def apply(in: Input): ParseResult[PString] = parser(in).map { str =>
      PString(str, in.offset, in.offset + str.length, Vector.empty)
    }
  }

  class ReplacingParser(parser: Parser[String], replacement: String, binding: Binding) extends Parser[PString] {
    def apply(in: Input): ParseResult[PString] = parser(in).map { str =>
      val mods = Vector(Modification(in.offset, -str.length, binding), Modification(in.offset, replacement.length, binding))
      PString(replacement, in.offset, in.offset + str.length, mods)
    }
  }

  implicit class stringExtensions(str: String) {
    def replaceWith(replacement: String, binding: Binding): Parser[PString] =
      new ReplacingParser(str, replacement, binding)

    def p: Parser[PString] =
      new ParserWithPos(str)

    def rp: Parser[PString] =
      new ParserWithPos(str.r)

    def bind(binding: Binding): Bound =
      new Bound(str, binding)
  }

  def join(pstrs: Traversable[PString]): PString =
    if (pstrs.nonEmpty) pstrs.reduce(_ + _) else PString("", 0, 0, Vector.empty)

  def withOffset[T](parser: Parser[T]): Parser[(T, Int)] = new Parser[(T, Int)] {
    override def apply(in: Input): ParseResult[(T, Int)] =
      parser.apply(in).map(r => (r, in.offset))
  }

}

object PositionTrackingParsers extends PositionTrackingParsers
