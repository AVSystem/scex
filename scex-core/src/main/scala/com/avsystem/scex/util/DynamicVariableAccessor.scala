package com.avsystem.scex
package util

import com.avsystem.scex.compiler.annotation.NotValidated

import scala.language.dynamics

/**
  * Created: 23-09-2013
  * Author: ghik
  */
class DynamicVariableAccessor[C <: ExpressionContext[_, V], V](ctx: C) extends TypedVariableAccessor(ctx) with Dynamic {
  @NotValidated def selectDynamic(name: String): V =
    ctx.getVariable(name)

  @NotValidated def updateDynamic(name: String)(value: V): Unit =
    ctx.setVariable(name, value)
}

class TypedVariableAccessor[C <: ExpressionContext[_, _]](val ctx: C) {
  @NotValidated protected def inferVarTag[T](implicit vt: ctx.VarTag[T]): ctx.VarTag[T] = vt
}
