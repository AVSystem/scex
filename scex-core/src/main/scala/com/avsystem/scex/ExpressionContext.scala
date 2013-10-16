package com.avsystem.scex

import com.avsystem.scex.compiler.annotation.NotValidated
import java.{util => ju, lang => jl}

/**
 * Created: 23-09-2013
 * Author: ghik
 */
trait ExpressionContext {
  type Root
  type Variable

  @NotValidated def root: Root

  def setVariable(name: String, value: Variable)

  def getVariable(name: String): Variable
}
