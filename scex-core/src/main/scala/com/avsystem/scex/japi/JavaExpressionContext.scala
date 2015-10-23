package com.avsystem.scex.japi

import com.avsystem.scex.{ExpressionContext, NoTag}

trait JavaExpressionContext[R, V] extends ExpressionContext {
  type Root = R
  type Var = V
  type VarTag[T] = NoTag

  def root: R

  def setVariable(name: String, value: V): Unit
  def getVariable(name: String): V

  def setTypedVariable[T](name: String, value: T)(implicit tag: NoTag): Unit
  def getTypedVariable[T](name: String)(implicit tag: NoTag): T
}
