package com.avsystem.scex
package compiler

import java.{lang => jl, util => ju}

/**
 * Created: 14-11-2013
 * Author: ghik
 */
case class ExpressionDef(
  profile: ExpressionProfile,
  template: Boolean,
  setter: Boolean,
  originalExpression: String,
  expression: String,
  positionMapping: PositionMapping,
  header: String,
  rootObjectClass: Class[_],
  contextType: String,
  resultType: String)
