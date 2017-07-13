package com.avsystem.scex.util

import com.avsystem.scex.NoTag
import com.avsystem.scex.japi.JavaExpressionContext

import scala.collection.mutable

/**
  * Created: 23-09-2013
  * Author: ghik
  */
case class SimpleContext[R](root: R) extends JavaExpressionContext[R, String] {
  private val variables = new mutable.HashMap[String, String]
  private val typedVariables = new mutable.HashMap[String, Any]

  def setVariable(name: String, value: String) =
    variables(name) = value

  def getVariable(name: String) =
    variables(name)

  def getTypedVariable[T](name: String)(implicit tag: NoTag): T =
    typedVariables(name).asInstanceOf[T]

  def setTypedVariable[T](name: String, value: T)(implicit tag: NoTag): Unit =
    typedVariables(name) = value
}
