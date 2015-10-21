package com.avsystem.scex.util

import java.{lang => jl, util => ju}

import com.avsystem.scex.ExpressionContext

import scala.collection.mutable

/**
  * Created: 23-09-2013
  * Author: ghik
  */
case class SimpleContext[R](root: R) extends ExpressionContext[R, String] {
  private val variables = new mutable.HashMap[String, String]
  private val typedVariables = new mutable.HashMap[String, Any]

  def setVariable(name: String, value: String) =
    variables(name) = value

  def getVariable(name: String) =
    variables(name)

  def getTypedVariable[T](name: String): T =
    typedVariables(name).asInstanceOf[T]

  def setTypedVariable[T](name: String, value: T): Unit =
    typedVariables(name) = value
}
