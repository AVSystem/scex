
import java.{lang => jl, util => ju}

import com.avsystem.scex.parsing.{ScalaParsingCommons, PositionTrackingParsers}

import scala.language.experimental.macros

object Playground extends ScalaParsingCommons with PositionTrackingParsers {

  private case class Part(expr: String)

  private case class Arg(expr: String, offset: Int)

  private val part = multilineInterpolationChars ^^ { s => Part(s.replaceAllLiterally("$$", "$"))}
  private val arg = withOffset(interpolationArg) ^^ { case (str, off) => Arg(str, off)}
  private val templateParser = rep(part ~ arg) ~ part ^^ {
    case pairs ~ lastPart => (pairs.map(_._1) :+ lastPart, pairs.map(_._2))
  }

  def main(args: Array[String]) {
    println(parseAll(templateParser, "abcd"))
    println(parseAll(templateParser, "abc$$d"))
    println(parseAll(templateParser, "abc$fuu"))
    println(parseAll(templateParser, "abc$fuu_+=haha"))
    println(parseAll(templateParser, "a.d.s.a.s$$bc${a.b.c.d.e}dafuq"))
  }

}
