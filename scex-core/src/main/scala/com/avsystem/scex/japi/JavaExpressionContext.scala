package com.avsystem.scex.japi

import com.avsystem.scex.{ExpressionContext, NoTag}

abstract class JavaExpressionContext[R, V] extends ExpressionContext[R, V] {
  type VarTag[T] = NoTag

  private type SuperSelf = ExpressionContext[R, V] {type VarTag[T] = NoTag}

  def setTypedVariable[T](name: String, value: T): Unit = (this: SuperSelf).setTypedVariable(name, value)(NoTag)
  def getTypedVariable[T](name: String): T = (this: SuperSelf).getTypedVariable(name)(NoTag)
}
