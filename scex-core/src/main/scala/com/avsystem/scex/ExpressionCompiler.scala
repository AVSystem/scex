package com.avsystem.scex

import java.{util => ju, lang => jl}
import scala.language.experimental.macros
import scala.reflect.runtime.{universe => ru}
import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter.IMain
import scala.reflect.ClassTag
import com.avsystem.scex.validation.ExpressionValidator
import scala.tools.nsc.interpreter.Results.Success
import scala.beans.BeanProperty


private object ExpressionCompiler {
  private val exprVal = "expr"

  private def wrapInCode(expression: String, from: ru.Type, to: ru.Type) =
    s"""
      |val $exprVal: ($from => $to) = { __ctx =>
      |  import __ctx._
      |  com.avsystem.scex.validation.ExpressionValidator.validate($expression)
      |}
    """.stripMargin
}

import ExpressionCompiler._

/**
 * Central class for expression compilation. Encapsulates Scala compiler, so it is
 * VERY heavy. Also, it is synchronized.
 */
class ExpressionCompiler {
  @BeanProperty var refreshAfter: Int = 500

  private val settings = new Settings
  settings.usejavacp.value = true

  private val interpreter = new IMain(settings)
  private var counter = 0

  def compileExpression[C <: AnyRef, T <: AnyRef](profile: ExpressionProfile, expression: String, cClass: Class[C], tClass: Class[T]): C => T = {
    implicit val cTag: ClassTag[C] = ClassTag(cClass)
    implicit val tTag: ClassTag[T] = ClassTag(tClass)

    compileExpression[C, T](profile, expression)
  }

  // TODO use TypeTags instead of ClassTags when scala reflection becomes threadsafe
  def compileExpression[C <: AnyRef : ClassTag, T <: AnyRef : ClassTag](profile: ExpressionProfile, expression: String): C => T =
    synchronized {
      val mirror = ru.runtimeMirror(getClass.getClassLoader)

      def erasedType[A: ClassTag] =
        mirror.classSymbol(implicitly[ClassTag[A]].runtimeClass).toType

      counter += 1
      if(counter >= refreshAfter) {
        interpreter.reset()
        counter = 0
      }

      ExpressionValidator.profile.withValue(profile) {
        interpreter.interpret(wrapInCode(expression, erasedType[C], erasedType[T])) match {
          case Success => interpreter.valueOfTerm(exprVal).get.asInstanceOf[C => T]
          case _ => null
        }
      }
    }
}
