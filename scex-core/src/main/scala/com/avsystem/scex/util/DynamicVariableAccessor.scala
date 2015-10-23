package com.avsystem.scex
package util

import java.{lang => jl, util => ju}

import com.avsystem.scex.compiler.annotation.NotValidated

import scala.language.dynamics

/**
  * Created: 23-09-2013
  * Author: ghik
  */
class DynamicVariableAccessor[C <: ExpressionContext](val ctx: C) extends Dynamic {
  @NotValidated def selectDynamic(name: String): ctx.Var =
    ctx.getVariable(name)

  @NotValidated def updateDynamic(name: String)(value: ctx.Var) =
    ctx.setVariable(name, value)

  @NotValidated protected def inferVarTag[T: ctx.VarTag] =
    implicitly[ctx.VarTag[T]]
}
