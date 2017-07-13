package com.avsystem.scex

import com.avsystem.scex.compiler.annotation.NotValidated

import scala.language.higherKinds

/**
  * Created: 23-09-2013
  * Author: ghik
  */
trait ExpressionContext[R, V] {
  type Root = R
  type Var = V
  type VarTag[T]

  @NotValidated def root: R

  def setVariable(name: String, value: V): Unit
  @NotValidated def getVariable(name: String): V

  def setTypedVariable[T](name: String, value: T)(implicit tag: VarTag[T]): Unit
  @NotValidated def getTypedVariable[T](name: String)(implicit tag: VarTag[T]): T
}

sealed trait NoTag
object NoTag extends NoTag {
  @NotValidated implicit val noTag: NoTag = this
}
