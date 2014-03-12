package com.avsystem.scex.compiler.presentation

import java.{util => ju, lang => jl}
import com.avsystem.scex.compiler.ExpressionDef
import com.google.common.cache.CacheBuilder
import java.util.concurrent.TimeUnit
import com.avsystem.scex.compiler.ScexCompiler.CompileError
import com.avsystem.scex.compiler.presentation.ScexPresentationCompiler.Completion
import com.avsystem.scex.util.CommonUtils

/**
 * Created: 12-12-2013
 * Author: ghik
 */
trait CachingScexPresentationCompiler extends ScexPresentationCompiler {
  import CommonUtils._

  private val errorsCache = CacheBuilder.newBuilder
    .expireAfterAccess(config.completionExpirationTime, TimeUnit.MILLISECONDS)
    .build[ExpressionDef, List[CompileError]]

  private val scopeCompletionCache = CacheBuilder.newBuilder
    .expireAfterAccess(config.completionExpirationTime, TimeUnit.MILLISECONDS)
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
