package com.avsystem.scex
package compiler

import com.avsystem.scex.compiler.ScexCompiler.CompilationFailedException
import com.avsystem.scex.parsing.PositionMapping
import com.avsystem.scex.validation.{SymbolValidator, SyntaxValidator}
import com.google.common.cache.CacheBuilder
import com.google.common.util.concurrent.ExecutionError

import java.util.concurrent.{ExecutionException, TimeUnit}
import scala.util.{Failure, Success, Try}

trait CachingScexCompiler extends ScexCompiler {

  import com.avsystem.scex.util.CommonUtils._

  private val preprocessingCache = CacheBuilder.newBuilder
    .expireAfterAccess(settings.expressionExpirationTime.value, TimeUnit.SECONDS)
    .maximumSize(settings.expressionCacheSize.value)
    .build[(String, Boolean), (String, PositionMapping)]

  private val expressionCache = CacheBuilder.newBuilder
    .expireAfterAccess(settings.expressionExpirationTime.value, TimeUnit.SECONDS)
    .maximumSize(settings.expressionCacheSize.value)
    .build[ExpressionDef, Try[RawExpression]]

  // holds names of packages to which profiles are compiled
  private val profileCompilationResultsCache =
    CacheBuilder.newBuilder.build[ExpressionProfile, Try[Option[String]]]

  // holds names of packages to which utils are compiled
  private val utilsCompilationResultsCache =
    CacheBuilder.newBuilder.build[String, Try[Option[String]]]

  // holds code of implicit adapters over Java classes that add Scala-style getters to Java bean getters
  private val javaGetterAdaptersCache =
    CacheBuilder.newBuilder.build[(String, String, Seq[Class[_]], Boolean), Try[Seq[Option[String]]]]

  private val syntaxValidatorsCache =
    CacheBuilder.newBuilder.build[String, SyntaxValidator]

  private val symbolValidatorsCache =
    CacheBuilder.newBuilder.build[String, SymbolValidator]

  private def invalidateCache(result: Try[_], invalidate: () => Unit): Unit = {
    if (!settings.cacheUnexpectedCompilationExceptions.value)
      result match {
        case Failure(_: CompilationFailedException) | Success(_) =>
        case Failure(_) => invalidate()
      }
  }

  override protected def preprocess(expression: String, template: Boolean) =
    unwrapExecutionException(
      preprocessingCache.get((expression, template), callable(super.preprocess(expression, template))))

  override protected def compileExpression(exprDef: ExpressionDef) = {
    val result = unwrapExecutionException(expressionCache.get(exprDef, callable(super.compileExpression(exprDef))))
    invalidateCache(result, () => expressionCache.invalidate(exprDef))

    result
  }

  override protected def compileProfileObject(profile: ExpressionProfile) = {
    val result = unwrapExecutionException(underLock(
      profileCompilationResultsCache.get(profile, callable(super.compileProfileObject(profile)))))
    invalidateCache(result, () => profileCompilationResultsCache.invalidate(profile))

    result
  }

  override protected def compileExpressionUtils(source: NamedSource) = {
    val result = unwrapExecutionException(underLock(
      utilsCompilationResultsCache.get(source.name, callable(super.compileExpressionUtils(source)))))
    invalidateCache(result, () => utilsCompilationResultsCache.invalidate(source.name))

    result
  }

  override protected def compileJavaGetterAdapters(profile: ExpressionProfile, name: String, classes: Seq[Class[_]], full: Boolean) =
    unwrapExecutionException(underLock(
      javaGetterAdaptersCache.get((profile.name, name, classes, full), callable(super.compileJavaGetterAdapters(profile, name, classes, full)))))

  override def compileSyntaxValidator(source: NamedSource) =
    unwrapExecutionException(
      syntaxValidatorsCache.get(source.name, callable(super.compileSyntaxValidator(source))))

  override def compileSymbolValidator(source: NamedSource) =
    unwrapExecutionException(
      symbolValidatorsCache.get(source.name, callable(super.compileSymbolValidator(source))))

  override def reset(): Unit = underLock {
    super.reset()
    expressionCache.invalidateAll()
    profileCompilationResultsCache.invalidateAll()
    utilsCompilationResultsCache.invalidateAll()
    javaGetterAdaptersCache.invalidateAll()
    syntaxValidatorsCache.invalidateAll()
    symbolValidatorsCache.invalidateAll()
  }

  private def unwrapExecutionException[T](code: => T) =
    try code catch {
      case e: ExecutionException => throw e.getCause
      case e: ExecutionError => throw e.getCause
    }
}
