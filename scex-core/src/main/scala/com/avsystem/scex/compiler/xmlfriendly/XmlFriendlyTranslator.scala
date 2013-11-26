package com.avsystem.scex
package compiler.xmlfriendly

import com.avsystem.scex.compiler.CodeGeneration
import java.{util => ju, lang => jl}

/**
 * Parser that translates XML-friendly expressions into correct scala code.
 * Implemented using Scala parser combinators (recursive descent parser).
 *
 * Created: 17-09-2013
 * Author: ghik
 */
object XmlFriendlyTranslator extends PositionTrackingParsers {
  val xmlFriendlyOperators = Map(
    "lt" -> "< ",
    "gt" -> "> ",
    "lte" -> "<= ",
    "gte" -> ">= ",
    "and" -> "&& ",
    "or" -> "||"
  ).withDefault(identity)

  def xmlFriendly(pstr: PString) =
    pstr.withResult(xmlFriendlyOperators(pstr.result))

  def translate(expr: String, template: Boolean = false) =
    parse(if (template) templateParser else expressionParser, expr).getOrElse(PString(expr, 0, expr.length, Vector.empty))

  val expressionParser = standardExpression ~ arbitraryEnding ^^ concat
  val templateParser = stringExpression ~ arbitraryEnding ^^ concat

  def stringExpression: Parser[PString] =
    rep("([^$]|\\$\\$)+".rp | interpolatedParam) ^^ join

  def interpolatedParam: Parser[PString] =
    "$".p ~ (ident | block) ^^ concat

  def standardExpression: Parser[PString] =
    rep(ident | btident | variable | stringlit | number | block | delim | operator | bracket | whitespace) ^^ join

  def ident: Parser[PString] =
    "[A-Za-z_][A-Za-z_0-9]*".rp ^^ xmlFriendly

  def btident: Parser[PString] =
    "`[^`]*`?".rp

  def stringlit: Parser[PString] =
    (("\'".p ~ stringlitContents ~ opt("\'".p)) | ("\"".p ~ stringlitContents ~ opt("\"".p))) ^^ {
      case beg ~ contents ~ endOpt =>
        beg.withResult("\"") + contents + endOpt.map(_.withResult("\""))
    }

  def stringlitContents: Parser[PString] =
    """([^"'\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*""".rp

  def number: Parser[PString] =
    "[0-9]+".rp

  def block = "{".p ~ standardExpression ~ opt("}".p) ^^ {
    case lb ~ contents ~ rbOpt => lb + contents + rbOpt
  }

  def operator: Parser[PString] =
    "[~!@#$%^&*-=+<>/?\\\\|:]+".rp ^^ xmlFriendly

  def delim = "[,;.]".rp

  def variable = "#".p ~> ident ^^ { id =>
    (CodeGeneration.VariablesSymbol + ".") ~+ id
  }

  def bracket: Parser[PString] =
    "[()\\[\\]}]".rp

  def whitespace: Parser[PString] =
    "\\s+".rp

  def arbitraryEnding: Parser[PString] =
    ".*$".rp

  override def skipWhitespace = false

  def concat(result: PString ~ PString) = result match {
    case first ~ second => first + second
  }

}
