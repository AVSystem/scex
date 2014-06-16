package com.avsystem.scex

import java.{lang => jl, util => ju}

import scala.runtime.AbstractFunction1
import scala.util.control.NonFatal

trait Expression[-C <: ExpressionContext[_, _], +T] extends (C => T) {
  @throws(classOf[EvaluationException])
  def apply(c: C): T

  def debugInfo: ExpressionDebugInfo
}

abstract class AbstractExpression[-C <: ExpressionContext[_, _], +T]
  extends AbstractFunction1[C, T] with Expression[C, T] {

  def eval(context: C): T

  final def apply(context: C): T = try eval(context) catch {
    case NonFatal(cause) =>
      val si = debugInfo.sourceInfo

      def isRelevant(el: StackTraceElement) =
        el.getFileName == si.sourceName && el.getLineNumber >= si.firstLine && el.getLineNumber < si.lastLine

      def lineInfo(el: StackTraceElement) = {
        val lineNumber = el.getLineNumber - si.firstLine
        (si.fullCode.substring(si.startOffset, si.endOffset).split('\n')(lineNumber), lineNumber + 1)
      }

      val (code, line) = Option(cause.getStackTrace).getOrElse(Array.empty).iterator
        .find(isRelevant).map(lineInfo).orNull

      throw new EvaluationException(code, line, cause)
  }

}
