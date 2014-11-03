package com.avsystem.scex

import java.{lang => jl, util => ju}

import com.avsystem.scex.compiler.ExpressionDef

/**
 * Created: 16-06-2014
 * Author: ghik
 */
class ExpressionDebugInfo(
  val definition: ExpressionDef) {

  lazy val originalLines = {
    val mapping = definition.positionMapping.reverse
    val lineLengths = definition.expression.split('\n').iterator.map(l => l.length + 1)
    val originalLineStarts = lineLengths.scanLeft(0)(_ + _).map(mapping.apply)

    originalLineStarts.sliding(2).map {
      case Seq(start, end) => definition.originalExpression.substring(start, end - 1)
    }.toVector
  }
}
