package com.avsystem.scex.compiler

import java.{util => ju, lang => jl}
import scala.util.parsing.combinator.RegexParsers

/**
 * Parser that translates XML-friendly expressions into correct scala code.
 * Implemented using Scala parser combinators (recursive descent parser).
 *
 * Created: 17-09-2013
 * Author: ghik
 */
object XmlFriendlyTranslator extends RegexParsers {
  val xmlFriendlyOperators = Map(
    "lt" -> "< ",
    "gt" -> "> ",
    "lte" -> "<= ",
    "gte" -> ">= ",
    "and" -> "&& ",
    "or" -> "||"
  ).withDefault(identity)

  def translate(expr: String) = parse(expression, expr).getOrElse(expr)

  val expression = concat((stringExpression | standardExpression) ~ arbitraryEnding)

  def stringExpression: Parser[String] =
    "raw\"\"\"" ~ rep("([^$]|\\$\\$)+".r | interpolatedParam) ~ opt("\"\"\"") ^^ {
      case beg ~ contents ~ endOpt => beg + contents.mkString + endOpt.getOrElse("")
    }

  def interpolatedParam: Parser[String] =
    concat("$" ~ (ident | block))

  def standardExpression: Parser[String] =
    rep(ident | btident | variable | stringlit | number | block | delim | operator | bracket | whitespace) ^^ (_.mkString)

  def ident: Parser[String] =
    "[A-Za-z_][A-Za-z_0-9]*".r ^^ xmlFriendlyOperators

  def btident: Parser[String] =
    "`[^`]*`?".r

  def stringlit: Parser[String] =
    (("\'" ~ stringlitContents ~ opt("\'")) | ("\"" ~ stringlitContents ~ opt("\""))) ^^ {
      case _ ~ contents ~ endOpt =>
        '\"' + contents + endOpt.map(_ => "\"").getOrElse("")
    }

  def stringlitContents: Parser[String] =
    """([^"'\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*""".r

  def number: Parser[String] =
    "[0-9]+".r

  def block = "{" ~ standardExpression ~ opt("}") ^^ {
    case lb ~ contents ~ rb => lb + contents + rb.getOrElse("")
  }

  def operator: Parser[String] =
    "[~!@#$%^&*-=+<>/?\\\\|:]+".r ^^ xmlFriendlyOperators

  def delim = "[,;.]".r

  def variable = "#" ~> ident ^^ CodeGeneration.variableAccess

  def bracket: Parser[String] =
    "[()\\[\\]}]".r

  def whitespace: Parser[String] =
    "\\s+".r

  def concat(parser: Parser[String ~ String]) = parser ^^ {
    case first ~ second => first + second
  }

  def arbitraryEnding: Parser[String] = ".*$".r

  override def skipWhitespace = false
}
