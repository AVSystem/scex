package com.avsystem.scex.parsing

/**
 * Created: 03-11-2014
 * Author: ghik
 */
object TemplateParser extends ScalaParsingCommons with PositionTrackingParsers {

  private val part = multilineInterpolationChars ^^ (_.replace("$$", "$"))
  private val arg = new ParserWithPos(interpolationArg)

  private val templateParser = rep(part ~ arg) ~ part ^^ {
    case pairs ~ lastPart => (pairs.map(_._1) :+ lastPart, pairs.map(_._2))
  }

  def parseTemplate(expr: String): ParseResult[(List[String], List[PString])] =
    parseAll(templateParser, expr)
}
