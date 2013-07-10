package com.avsystem.scex.compiler

import com.avsystem.scex.util.CommonUtils
import com.google.common.cache.CacheBuilder
import java.util.concurrent.TimeUnit
import java.{util => ju, lang => jl}
import scala.util.Try

trait CachingScexCompiler extends ScexCompiler {

  import CommonUtils._

  private val expressionCache = CacheBuilder.newBuilder
    .expireAfterAccess(config.expressionExpirationTime, TimeUnit.MILLISECONDS)
    .build[ExpressionDef, Try[RawExpression]]

  // holds names of packages to which profiles are compiled
  private val profileCompilationResultsCache =
    CacheBuilder.newBuilder.build[ExpressionProfile, Try[String]]

  // holds code of implicit adapters over Java classes that add Scala-style getters to Java bean getters
  private val fullJavaGetterAdaptersCache =
    CacheBuilder.newBuilder.build[Class[_], Try[String]]

  override protected def compileExpression(exprDef: ExpressionDef) =
    expressionCache.get(exprDef, callable(super.compileExpression(exprDef)))

  override protected def compileProfileObject(profile: ExpressionProfile) =
    profileCompilationResultsCache.get(profile, callable(super.compileProfileObject(profile)))

  override protected def compileFullJavaGetterAdapter(clazz: Class[_]) =
    fullJavaGetterAdaptersCache.get(clazz, callable(super.compileFullJavaGetterAdapter(clazz)))

  override def reset() {
    synchronized {
      super.reset()
      expressionCache.invalidateAll()
      profileCompilationResultsCache.invalidateAll()
      fullJavaGetterAdaptersCache.invalidateAll()
    }
  }
}
