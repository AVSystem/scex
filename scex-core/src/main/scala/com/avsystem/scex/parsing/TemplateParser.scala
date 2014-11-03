package com.avsystem.scex.parsing

import java.{lang => jl, util => ju}

/**
 * Created: 03-11-2014
 * Author: ghik
 */
object TemplateParser extends ScalaParsingCommons with PositionTrackingParsers {

  private val part = multilineInterpolationChars ^^ (_.replaceAllLiterally("$$", "$"))
  private val arg = new ParserWithPos(interpolationArg)

  private val templateParser = rep(part ~ arg) ~ part ^^ {
    case pairs ~ lastPart => (pairs.map(_._1) :+ lastPart, pairs.map(_._2))
  }

  def parseTemplate(expr: String) =
    parseAll(templateParser, expr)
}
