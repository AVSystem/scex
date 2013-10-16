package com.avsystem.scex.compiler

import JavaTypeParsing._
import com.avsystem.scex.compiler.ScexCompiler.{CompileError, CompilationFailedException}
import com.avsystem.scex.compiler.ScexPresentationCompiler.Param
import com.avsystem.scex.util.CacheImplicits
import com.avsystem.scex.{AbstractExpressionContext, Expression}
import com.google.common.cache.CacheBuilder
import com.google.common.reflect.TypeToken
import java.lang.reflect.Type
import java.{util => ju, lang => jl}

trait JavaScexCompiler extends ScexPresentationCompiler {

  import CacheImplicits._

  private val typesCache = CacheBuilder.newBuilder.weakKeys
    .build[Type, String](javaTypeAsScalaType _)

  private val rootObjectClassCache = CacheBuilder.newBuilder.weakKeys
    .build[TypeToken[_ <: AbstractExpressionContext[_, _]], Class[_]](getRootObjectClass _)

  private def getRootObjectClass(token: TypeToken[_ <: AbstractExpressionContext[_, _]]): Class[_] =
    token.getSupertype(classOf[AbstractExpressionContext[_, _]]).getType match {
      case ParameterizedType(_, _, Array(rootObjectType, _)) => TypeToken.of(rootObjectType).getRawType
      case clazz if clazz == classOf[AbstractExpressionContext[_, _]] => classOf[Object]
    }

  @throws[CompilationFailedException]
  def getCompiledStringExpression[C <: AbstractExpressionContext[_, _]](
    profile: ExpressionProfile,
    expression: String,
    contextClass: Class[C]): Expression[C, String] = {

    val rootObjectClass = rootObjectClassCache.get(TypeToken.of(contextClass))
    getCompiledStringExpressionByType(profile, expression, contextClass, rootObjectClass)
  }

  @throws[CompilationFailedException]
  def getCompiledStringExpression[C <: AbstractExpressionContext[_, _]](
    profile: ExpressionProfile,
    expression: String,
    contextTypeToken: TypeToken[C]): Expression[C, String] = {

    val rootObjectClass = rootObjectClassCache.get(contextTypeToken)
    getCompiledStringExpressionByType(profile, expression, contextTypeToken.getType, rootObjectClass)
  }

  @throws[CompilationFailedException]
  protected def getCompiledStringExpressionByType[C <: AbstractExpressionContext[_, _]](
    profile: ExpressionProfile,
    expression: String,
    contextType: Type,
    rootObjectClass: Class[_]): Expression[C, String] = {

    val scalaContextType = typesCache.get(contextType)

    getCompiledStringExpression(profile, expression, scalaContextType, rootObjectClass)
  }

  @throws[CompilationFailedException]
  def getCompiledExpression[C <: AbstractExpressionContext[_, _], R](
    profile: ExpressionProfile,
    expression: String,
    contextClass: Class[C],
    resultClass: Class[R]): Expression[C, R] = {

    val rootObjectClass = rootObjectClassCache.get(TypeToken.of(contextClass))
    getCompiledExpressionByTypes(profile, expression, contextClass, rootObjectClass, resultClass)
  }

  @throws[CompilationFailedException]
  def getCompiledExpression[C <: AbstractExpressionContext[_, _], R](
    profile: ExpressionProfile,
    expression: String,
    contextClass: Class[C],
    resultTypeToken: TypeToken[R]): Expression[C, R] = {

    val rootObjectClass = rootObjectClassCache.get(TypeToken.of(contextClass))
    getCompiledExpressionByTypes(profile, expression, contextClass, rootObjectClass, resultTypeToken.getType)
  }

  @throws[CompilationFailedException]
  def getCompiledExpression[C <: AbstractExpressionContext[_, _], R](
    profile: ExpressionProfile,
    expression: String,
    contextTypeToken: TypeToken[C],
    resultClass: Class[R]): Expression[C, R] = {

    val rootObjectClass = rootObjectClassCache.get(contextTypeToken)
    getCompiledExpressionByTypes(profile, expression, contextTypeToken.getType, rootObjectClass, resultClass)
  }

  @throws[CompilationFailedException]
  def getCompiledExpression[C <: AbstractExpressionContext[_, _], R](
    profile: ExpressionProfile,
    expression: String,
    contextTypeToken: TypeToken[C],
    resultTypeToken: TypeToken[R]): Expression[C, R] = {

    val rootObjectClass = rootObjectClassCache.get(contextTypeToken)
    getCompiledExpressionByTypes(profile, expression, contextTypeToken.getType, rootObjectClass, resultTypeToken.getType)
  }

  @throws[CompilationFailedException]
  protected def getCompiledExpressionByTypes[C <: AbstractExpressionContext[_, _], R](
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
    contextClass: Class[_ <: AbstractExpressionContext[_, _]],
    resultClass: Class[_]) = {

    val rootObjectClass = rootObjectClassCache.get(TypeToken.of(contextClass))
    getJavaInteractiveContextByTypes(profile, contextClass, rootObjectClass, resultClass)
  }

  def getJavaInteractiveContext(
    profile: ExpressionProfile,
    contextTypeToken: TypeToken[_ <: AbstractExpressionContext[_, _]],
    resultClass: Class[_]) = {

    val rootObjectClass = rootObjectClassCache.get(contextTypeToken)
    getJavaInteractiveContextByTypes(profile, contextTypeToken.getType, rootObjectClass, resultClass)
  }

  def getJavaInteractiveContext(
    profile: ExpressionProfile,
    contextClass: Class[_ <: AbstractExpressionContext[_, _]],
    resultTypeToken: TypeToken[_]) = {

    val rootObjectClass = rootObjectClassCache.get(TypeToken.of(contextClass))
    getJavaInteractiveContextByTypes(profile, contextClass, rootObjectClass, resultTypeToken.getType)
  }

  def getJavaInteractiveContext(
    profile: ExpressionProfile,
    contextTypeToken: TypeToken[_ <: AbstractExpressionContext[_, _]],
    resultTypeToken: TypeToken[_]) = {

    val rootObjectClass = rootObjectClassCache.get(contextTypeToken)
    getJavaInteractiveContextByTypes(profile, contextTypeToken.getType, rootObjectClass, resultTypeToken.getType)
  }

  protected def getJavaInteractiveContextByTypes(
    profile: ExpressionProfile,
    contextType: Type,
    rootObjectClass: Class[_],
    resultType: Type): JavaInteractiveContext = {

    new JavaInteractiveContext(getInteractiveContext(
      profile, typesCache.get(contextType), rootObjectClass, typesCache.get(resultType)))
  }
}

object JavaScexCompiler {
  def apply(compilerConfig: ScexCompilerConfig) =
    new DefaultJavaScexCompiler(compilerConfig)

  case class Member(getName: String, getParams: ju.Collection[ju.Collection[Param]],
    getType: String, isImplicit: Boolean)

  case class Completion(getMembers: ju.Collection[Member], getErrors: ju.Collection[CompileError])

}
