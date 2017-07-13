package com.avsystem.scex
package util

import com.avsystem.scex.compiler.annotation.NotValidated

import scala.language.dynamics

/**
  * Created: 23-09-2013
  * Author: ghik
  */
class DynamicVariableAccessor[C <: ExpressionContext[_, V], V](val ctx: C) extends Dynamic {
  @NotValidated def selectDynamic(name: String): V =
    ctx.getVariable(name)

  @NotValidated def updateDynamic(name: String)(value: V) =
    ctx.setVariable(name, value)

  @NotValidated protected def inferVarTag[T](implicit vt: ctx.VarTag[T]) = vt
}
