package com.avsystem.scex.parsing

import java.{lang => jl, util => ju}

import scala.reflect.internal.Chars
import scala.util.parsing.combinator.RegexParsers


/**
 * Created: 30-10-2014
 * Author: ghik
 */
trait ScalaParsingCommons extends RegexParsers {

  override def skipWhitespace = false

  protected class StringParseable[T] {
    def toString(t: T) = t.toString
  }

  protected object StringParseable {
    implicit val stringStringParseable = new StringParseable[String]
    implicit val elemStringParseable = new StringParseable[Elem]

    implicit def optionStringParseable[T: StringParseable] = new StringParseable[Option[T]] {
      override def toString(t: Option[T]) = t.map(implicitly[StringParseable[T]].toString).getOrElse("")
    }
  }

  protected implicit class StringParseableParserOps[L: StringParseable](parser: Parser[L]) {
    def ~~[R: StringParseable](other: Parser[R]): Parser[String] =
      parser ~ other ^^ { case left ~ right =>
        implicitly[StringParseable[L]].toString(left) + implicitly[StringParseable[R]].toString(right)
      }
  }

  protected implicit def strToParserOps(str: String): StringParseableParserOps[String] =
    new StringParseableParserOps(str)

  protected implicit def elemToParserOps(char: Elem): StringParseableParserOps[Elem] =
    new StringParseableParserOps(char)

  protected def join[T: StringParseable](elems: List[T]): String = {
    val parseable = implicitly[StringParseable[T]]
    elems.iterator.map(parseable.toString).mkString
  }

  protected def repjoin[T: StringParseable](parser: Parser[T]) =
    rep(parser) ^^ join[T]

  protected def rep1join[T: StringParseable](parser: Parser[T]) =
    rep1(parser) ^^ join[T]

  val opchar: Parser[Elem] =
    elem("opchar", Chars.isOperatorPart)

  val idstart: Parser[Elem] =
    elem("idstart", Chars.isIdentifierStart)

  val idpart: Parser[Elem] =
    elem("idpart", ch => Chars.isIdentifierPart(ch) && ch != '_')

  val underscore: Parser[Elem] =
    elem('_')

  val underscores: Parser[String] =
    "_+".r

  val operator: Parser[String] =
    rep1join(opchar)

  val plainIdent: Parser[String] =
    idstart ~~ repjoin(idpart | underscore)

  val ident: Parser[String] =
    idstart ~~ repjoin(idpart) ~~ repjoin(underscores ~~ rep1join(idpart)) ~~ opt(underscores ~~ repjoin(opchar))

  val btident: Parser[String] =
    """`[^`\p{Cntrl}]+`""".r

  val delim: Parser[String] =
    "[.,;]".r

  val number: Parser[String] =
    """(\d+(\.\d*)?|\d*\.\d+)""".r

  val escape: Parser[String] =
    """\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4}""".r

  val quotedChars: Parser[String] =
    """([^"\p{Cntrl}\\])+""".r

  val quotedInterpolationChars: Parser[String] =
    repjoin("""([^"\p{Cntrl}\$])+|\$\$""".r)

  val multilineChars: Parser[String] =
    """([^"\p{Cntrl}])+|"{1,2}(?!")""".r

  val multilineInterpolationChars: Parser[String] =
    repjoin("""([^"\p{Cntrl}\$])+|"{1,2}(?!")|\$\$""".r)

  val interpolationArg: Parser[String] =
    "$" ~~ (plainIdent | block)

  val charLiteral: Parser[String] =
    "'" ~~ ("""[^\p{Cntrl}\\]""" | escape) ~~ "'"

  val symbolLiteral: Parser[String] =
    "'" ~~ ident

  val stringLiteral: Parser[String] =
    "\"" ~~ repjoin(quotedChars | escape) ~~ "\""

  val stringInterpolation: Parser[String] =
    ident ~~ "\"" ~~ repjoin(quotedInterpolationChars ~~ interpolationArg) ~~ quotedInterpolationChars ~~ "\""

  val multilineEnd: Parser[String] =
    """\"{3,5}""".r

  val multilineString: Parser[String] =
    "\"\"\"" ~~ repjoin(multilineChars) ~~ multilineEnd

  val multilineInterpolation: Parser[String] =
    ident ~~ "\"\"\"" ~~ repjoin(multilineInterpolationChars ~~ interpolationArg) ~~ multilineInterpolationChars ~~ multilineEnd

  val whitespace: Parser[String] =
    """\s+""".r

  def block: Parser[String] =
    "{" ~~ expr ~~ "}"

  def brackets: Parser[String] =
    "[" ~~ expr ~~ "]"

  def parens: Parser[String] =
    "(" ~~ expr ~~ ")"

  def expr: Parser[String] =
    repjoin(whitespace | block | brackets | parens | delim | multilineInterpolation | multilineString |
      stringInterpolation | stringLiteral | charLiteral | symbolLiteral | number | delim | ident | btident | operator)

}

object ScalaParsingCommons extends ScalaParsingCommons
