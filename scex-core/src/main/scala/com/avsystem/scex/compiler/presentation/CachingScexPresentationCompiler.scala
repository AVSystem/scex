package com.avsystem.scex
package compiler.presentation

import java.util.concurrent.TimeUnit
import java.{lang => jl, util => ju}

import com.avsystem.scex.compiler.ExpressionDef
import com.avsystem.scex.compiler.ScexCompiler.CompileError
import com.avsystem.scex.compiler.presentation.ScexPresentationCompiler.Completion
import com.google.common.cache.CacheBuilder

/**
 * Created: 12-12-2013
 * Author: ghik
 */
trait CachingScexPresentationCompiler extends ScexPresentationCompiler {

  import com.avsystem.scex.util.CommonUtils._

  private val errorsCache = CacheBuilder.newBuilder
    .expireAfterAccess(settings.completionExpirationTime.value, TimeUnit.SECONDS)
    .build[ExpressionDef, List[CompileError]]

  private val scopeCompletionCache = CacheBuilder.newBuilder
    .expireAfterAccess(settings.completionExpirationTime.value, TimeUnit.SECONDS)
    .build[ExpressionDef, Completion]

  override protected def getErrors(exprDef: ExpressionDef) =
    errorsCache.get(exprDef, callable(super.getErrors(exprDef)))

  override protected def getScopeCompletion(exprDef: ExpressionDef) =
    scopeCompletionCache.get(exprDef, callable(super.getScopeCompletion(exprDef)))

  override def reset() = synchronized {
    super.reset()
    errorsCache.invalidateAll()
    scopeCompletionCache.invalidateAll()
  }
}
