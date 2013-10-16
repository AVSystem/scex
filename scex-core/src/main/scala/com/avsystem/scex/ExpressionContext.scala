package com.avsystem.scex

import com.avsystem.scex.compiler.annotation.NotValidated
import java.{util => ju, lang => jl}

/**
 * Created: 23-09-2013
 * Author: ghik
 */
trait ExpressionContext[R, V] {
  @NotValidated def root: R

  def setVariable(name: String, value: V)

  def getVariable(name: String): V
}
