package com.avsystem.scex
package compiler

import java.{lang => jl, util => ju}

import scala.ref.WeakReference
import scala.util.Try

/**
 * Wraps compiled expression into a wrapper that only holds weak reference to underlying expression.
 * This is to allow actually compiled expression classes to be GCed after the compiler is reset.
 * This trait should be used together with CachingScexCompiler to avoid recompilation of expressions
 * every time GC wipes out the weak reference.
 *
 * Created: 02-04-2014
 * Author: ghik
 */
trait WeakReferenceWrappingScexCompiler extends ScexCompiler {

  /**
   * Wrapper that avoids holding strong reference to actual compiled expression.
   */
  private class WeakExpressionWrapper(exprDef: ExpressionDef, initiallyWrapped: RawExpression) extends RawExpression {
    var expressionRef = new WeakReference(initiallyWrapped)

    private def wrappedExpression: RawExpression =
      expressionRef.get match {
        case Some(expr) => expr
        case None =>
          expressionRef = new WeakReference(actuallyCompileExpression(exprDef).get)
          wrappedExpression
      }

    def apply(context: ExpressionContext[_, _]) =
      wrappedExpression.apply(context)

    def debugInfo =
      wrappedExpression.debugInfo
  }

  private def actuallyCompileExpression(exprDef: ExpressionDef) =
    super.compileExpression(exprDef)

  override protected def compileExpression(exprDef: ExpressionDef): Try[RawExpression] =
    actuallyCompileExpression(exprDef).map(new WeakExpressionWrapper(exprDef, _))
}
