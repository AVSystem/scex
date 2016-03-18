package com.avsystem.scex

import java.{lang => jl, util => ju}

import com.avsystem.scex.compiler.SourceInfo

import scala.runtime.AbstractFunction1
import scala.util.control.NonFatal

trait Expression[-C <: ExpressionContext[_, _], +T] extends (C => T) {
  @throws(classOf[EvaluationException])
  def apply(c: C): T

  def debugInfo: ExpressionDebugInfo
}

abstract class AbstractExpression[-C <: ExpressionContext[_, _], +T]
  extends AbstractFunction1[C, T] with Expression[C, T] {

  def sourceInfo: SourceInfo

  def eval(context: C): T

  final def apply(context: C): T = try eval(context) catch {
    case NonFatal(cause) => throw new EvaluationException(cause)
  }

}
