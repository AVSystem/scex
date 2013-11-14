package com.avsystem.scex.japi

import com.avsystem.scex.compiler.JavaTypeParsing._
import com.avsystem.scex.compiler.ScexCompiler.CompileError
import com.avsystem.scex.compiler.ScexPresentationCompiler.Param
import com.avsystem.scex.compiler.{ExpressionDef, ScexCompiler, ScexCompilerConfig, ScexPresentationCompiler}
import com.avsystem.scex.util.{Fluent, CacheImplicits}
import com.avsystem.scex.{ExpressionProfile, ExpressionContext}
import com.google.common.cache.CacheBuilder
import com.google.common.reflect.TypeToken
import java.lang.reflect.Type
import java.{util => ju, lang => jl}

trait JavaScexCompiler extends ScexCompiler {
  this: ScexPresentationCompiler =>

  import CacheImplicits._

  private val typesCache = CacheBuilder.newBuilder.weakKeys
    .build[Type, String](javaTypeAsScalaType _)

  private val rootObjectClassCache = CacheBuilder.newBuilder.weakKeys
    .build[TypeToken[_ <: ExpressionContext[_, _]], Class[_]](getRootObjectClass _)

  private def getRootObjectClass(token: TypeToken[_ <: ExpressionContext[_, _]]): Class[_] =
    token.getSupertype(classOf[ExpressionContext[_, _]]).getType match {
      case ParameterizedType(_, _, Array(rootObjectType, _)) => TypeToken.of(rootObjectType).getRawType
      case clazz if clazz == classOf[ExpressionContext[_, _]] => classOf[Object]
    }

  def buildExpression[C <: ExpressionContext[_, _], R](contextTypeToken: TypeToken[C], resultTypeToken: TypeToken[R]) =
    new ExpressionBuilder[C, R](contextTypeToken, resultTypeToken)

  def buildExpression[C <: ExpressionContext[_, _], R](contextTypeToken: TypeToken[C], resultClass: Class[R]) =
    new ExpressionBuilder[C, R](contextTypeToken, TypeToken.of(resultClass))

  def buildExpression[C <: ExpressionContext[_, _], R](contextClass: Class[C], resultTypeToken: TypeToken[R]) =
    new ExpressionBuilder[C, R](TypeToken.of(contextClass), resultTypeToken)

  def buildExpression[C <: ExpressionContext[_, _], R](contextClass: Class[C], resultClass: Class[R]) =
    new ExpressionBuilder[C, R](TypeToken.of(contextClass), TypeToken.of(resultClass))

  class ExpressionBuilder[C <: ExpressionContext[_, _], R](contextTypeToken: TypeToken[C], resultTypeToken: TypeToken[R]) extends Fluent {
    require(contextTypeToken != null, "Context type cannot be null")
    require(resultTypeToken != null, "Result type cannot be null")

    private var _profile: ExpressionProfile = _
    private var _expression: String = _
    private var _template: Boolean = false
    private var _header: String = ""

    def get = {
      require(_profile != null, "Profile cannot be null")
      require(_expression != null, "Expression cannot be null")
      require(_header != null, "Header cannot be null")

      val scalaContextType = typesCache.get(contextTypeToken.getType)
      val scalaResultType = typesCache.get(resultTypeToken.getType)
      val rootObjectClass = rootObjectClassCache.get(contextTypeToken)

      getCompiledExpression[C, R](ExpressionDef(_profile, _template, _expression, _header,
        rootObjectClass, scalaContextType, scalaResultType))
    }

    def profile(profile: ExpressionProfile) = fluent {
      _profile = profile
    }

    def expression(expression: String) = fluent {
      _expression = expression
    }

    def template(template: Boolean) = fluent {
      _template = template
    }

    def additionalHeader(header: String) = fluent {
      _header = header
    }
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

  def buildInteractiveContext(contextTypeToken: TypeToken[_ <: ExpressionContext[_, _]], resultTypeToken: TypeToken[_]) =
    new InteractiveContextBuilder(contextTypeToken, resultTypeToken)

  def buildInteractiveContext(contextClass: Class[_ <: ExpressionContext[_, _]], resultTypeToken: TypeToken[_]) =
    new InteractiveContextBuilder(TypeToken.of(contextClass), resultTypeToken)

  def buildInteractiveContext(contextTypeToken: TypeToken[_ <: ExpressionContext[_, _]], resultClass: Class[_]) =
    new InteractiveContextBuilder(contextTypeToken, TypeToken.of(resultClass))

  def buildInteractiveContext(contextClass: Class[_ <: ExpressionContext[_, _]], resultClass: Class[_]) =
    new InteractiveContextBuilder(TypeToken.of(contextClass), TypeToken.of(resultClass))

  class InteractiveContextBuilder(
    contextTypeToken: TypeToken[_ <: ExpressionContext[_, _]], resultTypeToken: TypeToken[_]) extends Fluent {

    require(contextTypeToken != null, "Context type cannot be null")
    require(resultTypeToken != null, "Result type cannot be null")

    private var _profile: ExpressionProfile = _
    private var _template: Boolean = false
    private var _header: String = ""

    def get = {
      require(_profile != null, "Profile cannot be null")
      require(_header != null, "Header cannot be null")

      val scalaContextType = typesCache.get(contextTypeToken.getType)
      val scalaResultType = typesCache.get(resultTypeToken.getType)
      val rootObjectClass = rootObjectClassCache.get(contextTypeToken)

      new JavaInteractiveContext(getInteractiveContext(
        _profile, _template, _header, scalaContextType, rootObjectClass, scalaResultType))
    }

    def profile(profile: ExpressionProfile) = fluent {
      _profile = profile
    }

    def template(template: Boolean) = fluent {
      _template = template
    }

    def additionalHeader(header: String) = fluent {
      _header = header
    }
  }

}

object JavaScexCompiler {
  def apply(compilerConfig: ScexCompilerConfig) =
    new DefaultJavaScexCompiler(compilerConfig)

  case class Member(getName: String, getParams: ju.Collection[ju.Collection[Param]],
    getType: String, isImplicit: Boolean)

  case class Completion(getMembers: ju.Collection[Member], getErrors: ju.Collection[CompileError])

}
