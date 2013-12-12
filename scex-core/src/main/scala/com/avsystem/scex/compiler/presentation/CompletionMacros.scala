package com.avsystem.scex.compiler.presentation

import java.{util => ju, lang => jl}
import scala.util.DynamicVariable
import scala.tools.nsc.interactive.{Global => IGlobal}
import scala.language.experimental.macros
import scala.reflect.macros.Context

/**
 * Created: 12-12-2013
 * Author: ghik
 */
object CompletionMacros {
  val treeToInject: DynamicVariable[IGlobal#Tree] = new DynamicVariable(null)

  def injectTree: Any = macro injectTree_impl

  def injectTree_impl(c: Context): c.Expr[Any] =
    c.Expr[Any](treeToInject.value.asInstanceOf[c.universe.Tree])
}
