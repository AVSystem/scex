package com.avsystem.scex.compiler

import JavaTypeParsing._
import com.avsystem.scex.compiler.ScexCompiler.{CompileError, CompilationFailedException}
import com.avsystem.scex.compiler.ScexPresentationCompiler.Param
import com.avsystem.scex.util.CacheImplicits
import com.avsystem.scex.{TypeTag, Expression}
import com.google.common.cache.CacheBuilder
import java.lang.reflect.Type
import java.{util => ju, lang => jl}

trait JavaScexCompilerApi extends ScexPresentationCompiler {

  import CacheImplicits._

  private val typesCache = CacheBuilder.newBuilder.weakKeys
    .build[Type, String](javaTypeAsScalaType _)

  @throws[CompilationFailedException]
  def getCompiledStringExpression[C](
    profile: ExpressionProfile,
    expression: String,
    contextClass: Class[C]): Expression[C, String] = {

    getCompiledStringExpressionByType(profile, expression, contextClass)
  }

  @throws[CompilationFailedException]
  def getCompiledStringExpression[C](
    profile: ExpressionProfile,
    expression: String,
    contextType: TypeTag[C]): Expression[C, String] = {

    getCompiledStringExpressionByType(profile, expression, contextType)
  }

  @throws[CompilationFailedException]
  def getCompiledStringExpressionByType[C](
    profile: ExpressionProfile,
    expression: String,
    contextType: Type): Expression[C, String] = {

    val scalaContextType = typesCache.get(contextType)
    val contextClass = erasureOf(contextType)

    getCompiledStringExpression(profile, expression, scalaContextType, contextClass)
  }

  @throws[CompilationFailedException]
  def getCompiledExpression[C, R](
    profile: ExpressionProfile,
    expression: String,
    contextType: Class[C],
    resultType: Class[R]): Expression[C, R] = {

    getCompiledExpressionByTypes(profile, expression, contextType, resultType)
  }

  @throws[CompilationFailedException]
  def getCompiledExpression[C, R](
    profile: ExpressionProfile,
    expression: String,
    contextType: Class[C],
    resultType: TypeTag[R]): Expression[C, R] = {

    getCompiledExpressionByTypes(profile, expression, contextType, resultType)
  }

  @throws[CompilationFailedException]
  def getCompiledExpression[C, R](
    profile: ExpressionProfile,
    expression: String,
    contextType: TypeTag[C],
    resultType: Class[R]): Expression[C, R] = {

    getCompiledExpressionByTypes(profile, expression, contextType, resultType)
  }

  @throws[CompilationFailedException]
  def getCompiledExpression[C, R](
    profile: ExpressionProfile,
    expression: String,
    contextType: TypeTag[C],
    resultType: TypeTag[R]): Expression[C, R] = {

    getCompiledExpressionByTypes(profile, expression, contextType, resultType)
  }

  @throws[CompilationFailedException]
  def getCompiledExpressionByTypes[C, R](
    profile: ExpressionProfile,
    expression: String,
    contextType: Type,
    resultType: Type): Expression[C, R] = {

    val scalaContextType = typesCache.get(contextType)
    val contextClass = erasureOf(contextType)
    val scalaResultType = typesCache.get(resultType)

    getCompiledExpression[C, R](profile, expression, scalaContextType, contextClass, scalaResultType)
  }

  class JavaInteractiveContext(wrapped: InteractiveContext) {

    import scala.collection.JavaConverters._

    private def memberToJava(scalaMember: ScexPresentationCompiler.Member) = scalaMember match {
      case ScexPresentationCompiler.Member(name, params, tpe) =>
        JavaScexCompilerApi.Member(name, params.map(_.asJavaCollection).asJavaCollection, tpe)
    }

    private def completionToJava(scalaCompletion: ScexPresentationCompiler.Completion) = scalaCompletion match {
      case ScexPresentationCompiler.Completion(members, errors) =>
        JavaScexCompilerApi.Completion(members.map(memberToJava).asJavaCollection, errors.asJavaCollection)
    }

    def getErrorsForJava(expression: String) =
      wrapped.getErrors(expression).asJavaCollection

    def getScopeCompletionForJava(expression: String, position: Int) =
      completionToJava(wrapped.getScopeCompletion(expression, position))

    def getTypeCompletionForJava(expression: String, position: Int) =
      completionToJava(wrapped.getTypeCompletion(expression, position))
  }

  def getJavaInteractiveContext(
    profile: ExpressionProfile,
    contextType: Type,
    resultType: Type): JavaInteractiveContext = {

    new JavaInteractiveContext(getInteractiveContext(
      profile, typesCache.get(contextType), erasureOf(contextType), typesCache.get(resultType)))
  }
}

object JavaScexCompilerApi {
  def apply(compilerConfig: ScexCompilerConfig) =
    new JavaScexCompiler(compilerConfig)

  case class Member(name: String, params: ju.Collection[ju.Collection[Param]], tpe: String)

  case class Completion(members: ju.Collection[Member], errors: ju.Collection[CompileError])

}
