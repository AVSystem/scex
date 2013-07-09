package com.avsystem.scex.compiler

import JavaTypeParsing._
import com.avsystem.scex.compiler.ScexCompiler.{CompileError, CompilationFailedException}
import com.avsystem.scex.util.CacheImplicits
import com.avsystem.scex.{TypeTag, Expression}
import com.google.common.cache.CacheBuilder
import java.lang.reflect.Type
import java.{util => ju, lang => jl}
import com.avsystem.scex.compiler.ScexPresentationCompiler.Param

trait JavaScexCompiler extends ScexPresentationCompiler {

  import CacheImplicits._

  private val typesCache = CacheBuilder.newBuilder.weakKeys
    .build[Type, String](javaTypeAsScalaType _)

  @throws[CompilationFailedException]
  def getCompiledStringExpression[C](
    profile: ExpressionProfile,
    expression: String,
    contextClass: Class[C]): Expression[C, String] = {

    getCompiledStringExpression(profile, expression, contextClass: Type)
  }

  @throws[CompilationFailedException]
  def getCompiledStringExpression[C](
    profile: ExpressionProfile,
    expression: String,
    contextType: TypeTag[C]): Expression[C, String] = {

    getCompiledStringExpression(profile, expression, contextType: Type)
  }

  @throws[CompilationFailedException]
  private def getCompiledStringExpression[C](
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

    getCompiledExpression(profile, expression, contextType: Type, resultType: Type)
  }

  @throws[CompilationFailedException]
  def getCompiledExpression[C, R](
    profile: ExpressionProfile,
    expression: String,
    contextType: Class[C],
    resultType: TypeTag[R]): Expression[C, R] = {

    getCompiledExpression(profile, expression, contextType: Type, resultType: Type)
  }

  @throws[CompilationFailedException]
  def getCompiledExpression[C, R](
    profile: ExpressionProfile,
    expression: String,
    contextType: TypeTag[C],
    resultType: Class[R]): Expression[C, R] = {

    getCompiledExpression(profile, expression, contextType: Type, resultType: Type)
  }

  @throws[CompilationFailedException]
  def getCompiledExpression[C, R](
    profile: ExpressionProfile,
    expression: String,
    contextType: TypeTag[C],
    resultType: TypeTag[R]): Expression[C, R] = {

    getCompiledExpression(profile, expression, contextType: Type, resultType: Type)
  }

  @throws[CompilationFailedException]
  private def getCompiledExpression[C, R](
    profile: ExpressionProfile,
    expression: String,
    contextType: Type,
    resultType: Type): Expression[C, R] = {

    val scalaContextType = typesCache.get(contextType)
    val contextClass = erasureOf(contextType)
    val scalaResultType = typesCache.get(resultType)

    getCompiledExpression[C, R](profile, expression, scalaContextType, contextClass, scalaResultType)
  }

  class JavaInteractiveContext(profile: ExpressionProfile, contextType: Type, resultType: Type)
    extends InteractiveContext(profile, typesCache.get(contextType), erasureOf(contextType), typesCache.get(resultType)) {

    import scala.collection.JavaConverters._

    private def memberToJava(scalaMember: ScexPresentationCompiler.Member) = scalaMember match {
      case ScexPresentationCompiler.Member(name, params, tpe) =>
        JavaScexCompiler.Member(name, params.map(_.asJavaCollection).asJavaCollection, tpe)
    }

    private def completionToJava(scalaCompletion: ScexPresentationCompiler.Completion) = scalaCompletion match {
      case ScexPresentationCompiler.Completion(members, errors) =>
        JavaScexCompiler.Completion(members.map(memberToJava).asJavaCollection, errors.asJavaCollection)
    }

    def getErrorsForJava(expression: String) =
      getErrors(expression).asJavaCollection

    def getScopeCompletionForJava(expression: String, position: Int) =
      completionToJava(getScopeCompletion(expression, position))

    def getTypeCompletionForJava(expression: String, position: Int) =
      completionToJava(getTypeCompletion(expression, position))
  }

  def getInteractiveContext(profile: ExpressionProfile,
    contextType: Type,
    resultType: Type): JavaInteractiveContext = {

    new JavaInteractiveContext(profile, contextType, resultType)
  }
}

object JavaScexCompiler {
  def apply(compilerConfig: ScexCompilerConfig): JavaScexCompiler =
    new JavaScexCompiler with CachingScexCompiler {
      // must be lazy or inside early initializer
      lazy val config = compilerConfig
    }

  case class Member(name: String, params: ju.Collection[ju.Collection[Param]], tpe: String)

  case class Completion(members: ju.Collection[Member], errors: ju.Collection[CompileError])

}
