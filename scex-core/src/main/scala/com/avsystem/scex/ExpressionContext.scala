package com.avsystem.scex

import java.{lang => jl, util => ju}

import com.avsystem.scex.compiler.annotation.NotValidated

import scala.language.higherKinds

/**
  * Created: 23-09-2013
  * Author: ghik
  */
trait ExpressionContext {
  type Root
  type Var
  type VarTag[T]

  @NotValidated def root: Root

  def setVariable(name: String, value: Var): Unit
  @NotValidated def getVariable(name: String): Var

  def setTypedVariable[T](name: String, value: T)(implicit tag: VarTag[T]): Unit
  @NotValidated def getTypedVariable[T](name: String)(implicit tag: VarTag[T]): T
}

sealed trait NoTag
object NoTag extends NoTag {
  @NotValidated implicit val noTag: NoTag = this
}
