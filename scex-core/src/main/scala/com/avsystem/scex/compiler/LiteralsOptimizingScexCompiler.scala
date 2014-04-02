package com.avsystem.scex
package compiler

import java.{util => ju, lang => jl}
import scala.util.{Try, Success}
import com.avsystem.scex.util.Literal

/**
 * Avoids actual compilation of most simple template literal expressions by trying to parse them
 * immediately into resulting values.
 *
 * Created: 01-04-2014
 * Author: ghik
 */
trait LiteralsOptimizingScexCompiler extends ScexCompiler {

  private class LiteralExpression(value: Any) extends RawExpression {
    def apply(ctx: ExpressionContext[_, _]) = value
  }

  override protected def compileExpression(exprDef: ExpressionDef) = {
    val eligible =
      exprDef.template && !exprDef.setter && !exprDef.expression.contains("$")

    def parsedLiteral = if (eligible) {
      val lit = Literal(preprocess(exprDef).expression)
      exprDef.resultType match {
        case rt if JavaTypeParsing.stringSupertypes.contains(rt) => Some(lit.toString)
        case "Boolean" | "java.lang.Boolean" => Some(lit.toBoolean)
        case "Char" | "java.lang.Character" => Some(lit.toChar)
        case "Byte" | "java.lang.Byte" => Some(lit.toByte)
        case "Short" | "java.lang.Short" => Some(lit.toShort)
        case "Int" | "java.lang.Integer" => Some(lit.toInt)
        case "Long" | "java.lang.Long" => Some(lit.toLong)
        case "Float" | "java.lang.Float" => Some(lit.toFloat)
        case "Double" | "java.lang.Double" => Some(lit.toDouble)
        case _ => None
      }
    } else None

    Try(parsedLiteral).getOrElse(None)
      .map(value => Success(new LiteralExpression(value)))
      .getOrElse(super.compileExpression(exprDef))
  }
}
