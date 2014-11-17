package com.avsystem.scex
package util

import java.{lang => jl, util => ju}

import com.avsystem.scex.compiler.annotation.NotValidated

import scala.language.dynamics

/**
 * Created: 23-09-2013
 * Author: ghik
 */
class DynamicVariableAccessor[V](val context: ExpressionContext[_, V]) extends Dynamic {
  @NotValidated def selectDynamic(name: String): V =
    context.getVariable(name)

  @NotValidated def updateDynamic(name: String)(value: V) =
    context.setVariable(name, value)
}
