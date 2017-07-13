package com.avsystem.scex
package compiler

import com.avsystem.scex.parsing.PositionMapping

/**
  * Created: 14-11-2013
  * Author: ghik
  */
case class ExpressionDef(
  profile: ExpressionProfile,
  template: Boolean,
  setter: Boolean,
  expression: String,
  header: String,
  contextType: String,
  resultType: String,
  variableTypes: Map[String, String])(

  val originalExpression: String,
  val positionMapping: PositionMapping,
  val rootObjectClass: Class[_]) {
}
