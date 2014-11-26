package com.avsystem.scex
package compiler.presentation

import java.util.concurrent.{ExecutionException, TimeUnit}
import java.{lang => jl, util => ju}

import com.avsystem.scex.compiler.ExpressionDef
import com.avsystem.scex.compiler.ScexCompiler.{CompilationFailedException, CompileError}
import com.avsystem.scex.compiler.presentation.ScexPresentationCompiler.{Member, Completion}
import com.avsystem.scex.presentation.SymbolAttributes
import com.avsystem.scex.util.TypeWrapper
import com.google.common.cache.CacheBuilder

/**
 * Created: 12-12-2013
 * Author: ghik
 */
trait CachingScexPresentationCompiler extends ScexPresentationCompiler {

  import com.avsystem.scex.util.CommonUtils._

  private val errorsCache = CacheBuilder.newBuilder
    .expireAfterAccess(settings.completionExpirationTime.value, TimeUnit.SECONDS)
    .maximumSize(settings.errorsCacheSize.value)
    .build[ExpressionDef, List[CompileError]]

  private val scopeCompletionCache = CacheBuilder.newBuilder
    .expireAfterAccess(settings.completionExpirationTime.value, TimeUnit.SECONDS)
    .maximumSize(settings.scopeCompletionCacheSize.value)
    .build[ExpressionDef, Completion]

  case class TypeMembersCacheKey(profile: ExpressionProfile, contextType: String, ownerType: TypeWrapper)

  private val typeMembersCache = CacheBuilder.newBuilder
    .expireAfterAccess(settings.completionExpirationTime.value, TimeUnit.SECONDS)
    .maximumSize(settings.typeMembersCacheSize.value)
    .build[TypeMembersCacheKey, Vector[Member]]

  private val symbolAttributesCache = CacheBuilder.newBuilder
    .build[String, SymbolAttributes]

  override protected def getErrors(exprDef: ExpressionDef) =
    errorsCache.get(exprDef, callable(super.getErrors(exprDef)))

  override protected def getScopeCompletion(exprDef: ExpressionDef) =
    scopeCompletionCache.get(exprDef, callable(super.getScopeCompletion(exprDef)))

  override protected def getTypeMembers(global: IGlobal)(exprDef: ExpressionDef, ownerTpe: global.Type)
    (computeMembers: => Vector[Member]): Vector[Member] = {

    val key = TypeMembersCacheKey(exprDef.profile, exprDef.contextType, TypeWrapper(global)(ownerTpe.map(_.widen)))
    typeMembersCache.get(key, callable(computeMembers))
  }

  override def compileSymbolAttributes(source: NamedSource) =
    unwrapExecutionException(symbolAttributesCache.get(source.name, callable(super.compileSymbolAttributes(source))))

  override def reset() = synchronized {
    super.reset()
    errorsCache.invalidateAll()
    scopeCompletionCache.invalidateAll()
    typeMembersCache.invalidateAll()
    symbolAttributesCache.invalidateAll()
  }

  private def unwrapExecutionException[T](code: => T) =
    try code catch {
      case e: ExecutionException => throw e.getCause
    }
}
