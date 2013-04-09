package com.avsystem.scex

import java.{util => ju, lang => jl}
import scala.reflect.runtime.{universe => ru}
import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter.IMain
import scala.reflect.ClassTag
import scala.tools.nsc.interpreter.Results.Success
import scala.beans.BeanProperty
import scala.collection.JavaConverters._
import com.avsystem.scex.validation.ExpressionValidator
import java.util.concurrent.ConcurrentHashMap

private object ExpressionCompiler {
  private val exprVal = "expr"

  private case class ExpressionDef(
    profile: ExpressionProfile,
    expression: String,
    contextClass: Class[_],
    resultClass: Class[_]) {
  }

  private type RawExpression = Function[_, _]
}

import ExpressionCompiler._

/**
 * Central class for expression compilation. Encapsulates Scala compiler, so it is
 * VERY heavy. Also, it is synchronized.
 */
class ExpressionCompiler {
  @BeanProperty var resetAfter: Int = 500

  private val settings = new Settings
  settings.usejavacp.value = true

  private val interpreter = new IMain(settings)


  private val expressionCache = new ConcurrentHashMap[ExpressionDef, RawExpression].asScala

  def getCompiledExpression[C <: AnyRef : ClassTag, R <: AnyRef : ClassTag](
    profile: ExpressionProfile,
    expression: String): C => R = {

    def clazz[T: ClassTag] = implicitly[ClassTag[T]].runtimeClass.asInstanceOf[Class[T]]

    getCompiledExpression[C, R](profile, expression, clazz[C], clazz[R])
  }

  /**
   * Returns compiled expression ready to be evaluated. Returned expression is actually a proxy that
   * looks up ExpressionCompiler's cache on every access (evaluation).
   *
   * @param profile expression profile to compile this expression with
   * @param expression the expression
   * @param contextClass expression context class
   * @param resultClass expression result class
   */
  def getCompiledExpression[C <: AnyRef, R <: AnyRef](
    profile: ExpressionProfile,
    expression: String,
    contextClass: Class[C],
    resultClass: Class[R]): C => R = {

    require(profile != null, "Profile cannot be null")
    require(expression != null, "Expression cannot be null")
    require(contextClass != null, "Context class cannot be null")
    require(resultClass != null, "Result class cannot be null")

    val exprDef = ExpressionDef(profile, expression, contextClass, resultClass)

    // force eager compilation
    getUsingCache(exprDef)
    // return a proxy that will fetch compiled expression from ExpressionCompiler at each access
    (context: C) => getUsingCache(exprDef).asInstanceOf[C => R](context)
  }

  /**
   * Manages cache. Returns cached expressions, puts into cache, clears cache and resets interpreter when needed.
   */
  private def getUsingCache(exprDef: ExpressionDef): RawExpression = {
    var result = expressionCache(exprDef)

    if (result == null) {
      synchronized {
        result = expressionCache(exprDef)
        if (result == null) {
          val reset = expressionCache.size > resetAfter
          if (reset) {
            interpreter.reset()
          }
          result = compileExpression(exprDef)
          if (reset) {
            expressionCache.clear()
          }
          if (result != null) {
            expressionCache(exprDef) = result
          }
        }
      }
    }

    result
  }

  /**
   * Actually uses interpreter to compile given expression into bytecode.
   */
  private def compileExpression(exprDef: ExpressionDef): RawExpression = {
    val mirror = ru.runtimeMirror(getClass.getClassLoader)

    def erasedType[A](clazz: Class[A]) =
      mirror.classSymbol(clazz).toType

    val codeToCompile =
      s"""
        |val $exprVal: (${erasedType(exprDef.contextClass)} => ${erasedType(exprDef.resultClass)}) = { __ctx =>
        |  import __ctx._
        |  com.avsystem.scex.validation.ExpressionValidator.validate(${exprDef.expression})
        |}
      """.stripMargin

    ExpressionValidator.profile.withValue(exprDef.profile) {
      interpreter.interpret(codeToCompile) match {
        case Success => interpreter.valueOfTerm(exprVal).get.asInstanceOf[RawExpression]
        case _ => null
      }
    }
  }
}
