package com.avsystem.scex.util

import java.{lang => jl, util => ju}

import com.avsystem.scex.japi.JavaExpressionContext

import scala.collection.mutable

/**
  * Created: 23-09-2013
  * Author: ghik
  */
case class SimpleContext[R](private val scalaRoot: R) extends JavaExpressionContext[R, String] {
  private val variables = new mutable.HashMap[String, String]
  private val typedVariables = new mutable.HashMap[String, Any]

  def getRoot = scalaRoot

  def setVariableFromJava(name: String, value: String) =
    variables(name) = value

  def getVariableFromJava(name: String) =
    variables(name)

  def getTypedVariableFromJava[T](name: String): T =
    typedVariables(name).asInstanceOf[T]

  def setTypedVariableFromJava[T](name: String, value: T): Unit =
    typedVariables(name) = value
}
