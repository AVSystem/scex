package com.avsystem.scex.japi

import com.avsystem.scex.{ExpressionContext, NoTag}

abstract class JavaExpressionContext[R, V] extends ExpressionContext {
  type Root = R
  type Var = V
  type VarTag[T] = NoTag

  def root: R = getRoot
  def getRoot: R

  def getVariable(name: String): V = getVariableFromJava(name)
  def getVariableFromJava(name: String): V

  def setVariable(name: String, value: V): Unit = setVariableFromJava(name, value)
  def setVariableFromJava(name: String, value: V): Unit

  def setTypedVariable[T](name: String, value: T)(implicit tag: NoTag): Unit = setTypedVariableFromJava(name, value)
  def setTypedVariableFromJava[T](name: String, value: T): Unit

  def getTypedVariable[T](name: String)(implicit tag: NoTag): T = getTypedVariableFromJava(name)
  def getTypedVariableFromJava[T](name: String): T
}
