package com.avsystem.scex
package util

import com.avsystem.scex.ExpressionContext
import java.{util => ju, lang => jl}
import scala.collection.mutable

/**
 * Created: 23-09-2013
 * Author: ghik
 */
case class SimpleContext[R](root: R) extends ExpressionContext[R, String] {
  private val variables = new mutable.HashMap[String, String]

  def setVariable(name: String, value: String) =
    variables(name) = value

  def getVariable(name: String) =
    variables(name)
}