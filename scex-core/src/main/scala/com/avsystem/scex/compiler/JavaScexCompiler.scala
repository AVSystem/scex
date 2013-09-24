package com.avsystem.scex.compiler

import JavaTypeParsing._
import com.avsystem.scex.compiler.ScexCompiler.{CompileError, CompilationFailedException}
import com.avsystem.scex.compiler.ScexPresentationCompiler.Param
import com.avsystem.scex.util.CacheImplicits
import com.avsystem.scex.{ExpressionContext, TypeTag, Expression}
import com.google.common.cache.CacheBuilder
import java.lang.reflect.Type
import java.{util => ju, lang => jl}

trait JavaScexCompiler extends ScexPresentationCompiler {

  import CacheImplicits._

  private val typesCache = CacheBuilder.newBuilder.weakKeys
    .build[Type, String](javaTypeAsScalaType _)

  @throws[CompilationFailedException]
  def getCompiledStringExpression[C <: ExpressionContext](
    profile: ExpressionProfile,
    expression: String,
    contextClass: Class[C],
    rootObjectClass: Class[_]): Expression[C, String] = {

    getCompiledStringExpressionByType(profile, expression, contextClass, rootObjectClass)
  }

  @throws[CompilationFailedException]
  def getCompiledStringExpression[C <: ExpressionContext](
    profile: ExpressionProfile,
    expression: String,
    contextType: TypeTag[C],
    rootObjectClass: Class[_]): Expression[C, String] = {

    getCompiledStringExpressionByType(profile, expression, contextType, rootObjectClass)
  }

  @throws[CompilationFailedException]
  def getCompiledStringExpressionByType[C <: ExpressionContext](
    profile: ExpressionProfile,
    expression: String,
    contextType: Type,
    rootObjectClass: Class[_]): Expression[C, String] = {

    val scalaContextType = typesCache.get(contextType)

    getCompiledStringExpression(profile, expression, scalaContextType, rootObjectClass)
  }

  @throws[CompilationFailedException]
  def getCompiledExpression[C <: ExpressionContext, R](
    profile: ExpressionProfile,
    expression: String,
    contextType: Class[C],
    rootObjectClass: Class[_],
    resultType: Class[R]): Expression[C, R] = {

    getCompiledExpressionByTypes(profile, expression, contextType, rootObjectClass, resultType)
  }

  @throws[CompilationFailedException]
  def getCompiledExpression[C <: ExpressionContext, R](
    profile: ExpressionProfile,
    expression: String,
    contextType: Class[C],
    rootObjectClass: Class[_],
    resultType: TypeTag[R]): Expression[C, R] = {

    getCompiledExpressionByTypes(profile, expression, contextType, rootObjectClass, resultType)
  }

  @throws[CompilationFailedException]
  def getCompiledExpression[C <: ExpressionContext, R](
    profile: ExpressionProfile,
    expression: String,
    contextType: TypeTag[C],
    rootObjectClass: Class[_],
    resultType: Class[R]): Expression[C, R] = {

    getCompiledExpressionByTypes(profile, expression, contextType, rootObjectClass, resultType)
  }

  @throws[CompilationFailedException]
  def getCompiledExpression[C <: ExpressionContext, R](
    profile: ExpressionProfile,
    expression: String,
    contextType: TypeTag[C],
    rootObjectClass: Class[_],
    resultType: TypeTag[R]): Expression[C, R] = {

    getCompiledExpressionByTypes(profile, expression, contextType, rootObjectClass, resultType)
  }

  @throws[CompilationFailedException]
  def getCompiledExpressionByTypes[C <: ExpressionContext, R](
    profile: ExpressionProfile,
    expression: String,
    contextType: Type,
    rootObjectClass: Class[_],
    resultType: Type): Expression[C, R] = {

    val scalaContextType = typesCache.get(contextType)
    val scalaResultType = typesCache.get(resultType)

    getCompiledExpression[C, R](profile, expression, scalaContextType, rootObjectClass, scalaResultType)
  }

  class JavaInteractiveContext(wrapped: InteractiveContext) {

    import scala.collection.JavaConverters._

    private def memberToJava(scalaMember: ScexPresentationCompiler.Member) = scalaMember match {
      case ScexPresentationCompiler.Member(name, params, tpe, implicitlyAdded) =>
        JavaScexCompiler.Member(name, params.map(_.asJavaCollection).asJavaCollection, tpe, implicitlyAdded)
    }

    private def completionToJava(scalaCompletion: ScexPresentationCompiler.Completion) = scalaCompletion match {
      case ScexPresentationCompiler.Completion(members, errors) =>
        JavaScexCompiler.Completion(members.map(memberToJava).asJavaCollection, errors.asJavaCollection)
    }

    def getErrors(expression: String) =
      wrapped.getErrors(expression).asJavaCollection

    def getScopeCompletion(expression: String, position: Int) =
      completionToJava(wrapped.getScopeCompletion(expression, position))

    def getTypeCompletion(expression: String, position: Int) =
      completionToJava(wrapped.getTypeCompletion(expression, position))
  }

  def getJavaInteractiveContext(
    profile: ExpressionProfile,
    contextType: Type,
    rootObjectClass: Class[_],
    resultType: Type): JavaInteractiveContext = {

    new JavaInteractiveContext(getInteractiveContext(
      profile, typesCache.get(contextType), erasureOf(contextType), typesCache.get(resultType)))
  }
}

object JavaScexCompiler {
  def apply(compilerConfig: ScexCompilerConfig) =
    new DefaultJavaScexCompiler(compilerConfig)

  case class Member(getName: String, getParams: ju.Collection[ju.Collection[Param]],
    getType: String, isImplicit: Boolean)

  case class Completion(getMembers: ju.Collection[Member], getErrors: ju.Collection[CompileError])

}
