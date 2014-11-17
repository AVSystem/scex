package com.avsystem.scex

import java.{lang => jl, util => ju}

import com.avsystem.scex.compiler.annotation.NotValidated

/**
 * Created: 23-09-2013
 * Author: ghik
 */
trait ExpressionContext[R, V] {
  @NotValidated def root: R

  def setVariable(name: String, value: V)

  def getVariable(name: String): V
}
