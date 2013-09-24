package com.avsystem.scex.util

import com.avsystem.scex.ExpressionContext
import com.avsystem.scex.compiler.annotation.NotValidated
import java.{util => ju, lang => jl}
import scala.language.dynamics

/**
 * Created: 23-09-2013
 * Author: ghik
 */
class DynamicVariableAccessor[V](val context: ExpressionContext {type Variable = V}) extends Dynamic {
  type Variable = context.Variable

  @NotValidated def selectDynamic(name: String): V =
    context.getVariable(name)

  @NotValidated def updateDynamic(name: String)(value: V) =
    context.setVariable(name, value)
}
