package com.avsystem.scex
package japi

import java.lang.reflect.Type
import java.{lang => jl, util => ju}

import com.avsystem.scex.compiler.JavaTypeParsing._
import com.avsystem.scex.compiler.presentation.ScexPresentationCompiler
import com.avsystem.scex.compiler.presentation.ScexPresentationCompiler.{Param, Type => SType}
import com.avsystem.scex.compiler.presentation.ast.Tree
import com.avsystem.scex.compiler.{ExpressionDef, ScexCompiler}
import com.avsystem.scex.util.Fluent
import com.google.common.cache.CacheBuilder
import com.google.common.reflect.TypeToken

trait JavaScexCompiler extends ScexCompiler {
  this: ScexPresentationCompiler =>

  import com.avsystem.scex.util.CacheImplicits._

  private val typesCache = CacheBuilder.newBuilder.weakKeys
    .build[Type, String](javaTypeAsScalaType _)

  private val rootObjectClassCache = CacheBuilder.newBuilder.weakKeys
    .build[TypeToken[_ <: ExpressionContext[_, _]], Class[_]](getRootObjectClass _)

  private def getRootObjectClass(token: TypeToken[_ <: ExpressionContext[_, _]]): Class[_] =
    token.getSupertype(classOf[ExpressionContext[_, _]]).getType match {
      case ParameterizedType(_, _, Array(rootObjectType, _)) => rootObjectType match {
        case TypeAny | TypeAnyVal => null
        case _ => TypeToken.of(rootObjectType).getRawType
      }
      case clazz if clazz == classOf[ExpressionContext[_, _]] => classOf[Object]
    }

  def buildExpression: ExpressionBuilder[_, _] =
    new ExpressionBuilder[ExpressionContext[_, _], Any]

  class ExpressionBuilder[C <: ExpressionContext[_, _], T] extends Fluent {
    private var _contextTypeToken: TypeToken[_ <: ExpressionContext[_, _]] = _
    private var _resultTypeToken: TypeToken[_] = _
    private var _profile: ExpressionProfile = _
    private var _expression: String = _
    private var _template: Boolean = true
    private var _setter: Boolean = false
    private var _header: String = ""

    def get = {
      require(_contextTypeToken != null, "Context type cannot be null")
      require(_resultTypeToken != null, "Result type cannot be null")
      require(_profile != null, "Profile cannot be null")
      require(_expression != null, "Expression cannot be null")
      require(_header != null, "Header cannot be null")

      val scalaContextType = typesCache.get(_contextTypeToken.getType)
      val scalaResultType = typesCache.get(_resultTypeToken.getType)
      val rootObjectClass = rootObjectClassCache.get(_contextTypeToken)

      val (actualExpression, positionMapping) = preprocess(_expression, _template)
      getCompiledExpression[C, T](ExpressionDef(_profile, _template, _setter, actualExpression, _header,
        scalaContextType, scalaResultType)(_expression, positionMapping, rootObjectClass))
    }

    def contextType[NC <: ExpressionContext[_, _]](contextTypeToken: TypeToken[NC]) = fluent {
      _contextTypeToken = contextTypeToken
    }.asInstanceOf[ExpressionBuilder[NC, T]]

    def contextType[NC <: ExpressionContext[_, _]](contextClass: Class[NC]) = fluent {
      _contextTypeToken = TypeToken.of(contextClass)
    }.asInstanceOf[ExpressionBuilder[NC, T]]

    def resultType[NT](resultTypeToken: TypeToken[NT]) = fluent {
      _resultTypeToken = resultTypeToken
      _setter = false
    }.asInstanceOf[ExpressionBuilder[C, NT]]

    def resultType[NT](resultClass: Class[NT]) = fluent {
      _resultTypeToken = TypeToken.of(resultClass)
      _setter = false
    }.asInstanceOf[ExpressionBuilder[C, NT]]

    def setterFor[NT](resultTypeToken: TypeToken[NT]) = fluent {
      _resultTypeToken = resultTypeToken
      _setter = true
    }.asInstanceOf[ExpressionBuilder[C, Setter[NT]]]

    def setterFor[NT](resultClass: Class[NT]) = fluent {
      _resultTypeToken = TypeToken.of(resultClass)
      _setter = true
    }.asInstanceOf[ExpressionBuilder[C, Setter[NT]]]

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

  class JavaCompleter(wrapped: Completer) {

    import scala.collection.JavaConverters._

    private def memberToJava(scalaMember: ScexPresentationCompiler.Member) = scalaMember match {
      case ScexPresentationCompiler.Member(name, params, tpe, implicitlyAdded, doc) =>
        JavaScexCompiler.Member(name, params.map(_.asJavaCollection).asJavaCollection, tpe, implicitlyAdded, doc.orNull)
    }

    private def completionToJava(scalaCompletion: ScexPresentationCompiler.Completion) = scalaCompletion match {
      case ScexPresentationCompiler.Completion(typedPrefixTree, members) =>
        JavaScexCompiler.Completion(typedPrefixTree, members.map(memberToJava).asJavaCollection)
    }

    def getErrors(expression: String) =
      wrapped.getErrors(expression).asJavaCollection

    def getScopeCompletion =
      completionToJava(wrapped.getScopeCompletion)

    def getTypeCompletion(expression: String, position: Int) =
      completionToJava(wrapped.getTypeCompletion(expression, position))

    def parse(expression: String) =
      wrapped.parse(expression)
  }

  def buildCompleter =
    new CompleterBuilder

  class CompleterBuilder extends Fluent {
    private var _contextTypeToken: TypeToken[_ <: ExpressionContext[_, _]] = _
    private var _resultTypeToken: TypeToken[_] = _
    private var _profile: ExpressionProfile = _
    private var _template: Boolean = true
    private var _setter: Boolean = false
    private var _header: String = ""

    def get = {
      require(_contextTypeToken != null, "Context type cannot be null")
      require(_resultTypeToken != null, "Result type cannot be null")
      require(_profile != null, "Profile cannot be null")
      require(_header != null, "Header cannot be null")

      val scalaContextType = typesCache.get(_contextTypeToken.getType)
      val scalaResultType = typesCache.get(_resultTypeToken.getType)
      val rootObjectClass = rootObjectClassCache.get(_contextTypeToken)

      new JavaCompleter(getCompleter(
        _profile, _template, _setter, _header, scalaContextType, rootObjectClass, scalaResultType))
    }

    def contextType(contextTypeToken: TypeToken[_ <: ExpressionContext[_, _]]) = fluent {
      _contextTypeToken = contextTypeToken
    }

    def contextType(contextClass: Class[_ <: ExpressionContext[_, _]]) = fluent {
      _contextTypeToken = TypeToken.of(contextClass)
    }

    def resultType(resultTypeToken: TypeToken[_]) = fluent {
      _resultTypeToken = resultTypeToken
      _setter = false
    }

    def resultType(resultClass: Class[_]) = fluent {
      _resultTypeToken = TypeToken.of(resultClass)
      _setter = false
    }

    def setterForType(resultTypeToken: TypeToken[_]) = fluent {
      _resultTypeToken = _resultTypeToken
      _setter = true
    }

    def setterForType(resultClass: Class[_]) = fluent {
      _resultTypeToken = TypeToken.of(resultClass)
      _setter = true
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

  case class Member(getName: String, getParams: ju.Collection[ju.Collection[Param]],
    getType: SType, isImplicit: Boolean, getDocumentation: String)

  case class Completion(getTypedPrefixTree: Tree, getMembers: ju.Collection[Member])

}
