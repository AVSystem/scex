package com.avsystem.scex
package compiler

import com.avsystem.scex.ExpressionProfile
import java.{util => ju, lang => jl}

/**
 * Created: 14-11-2013
 * Author: ghik
 */
case class ExpressionDef(
  profile: ExpressionProfile,
  template: Boolean,
  setter: Boolean,
  expression: String,
  positionMapping: PositionMapping,
  header: String,
  rootObjectClass: Class[_],
  contextType: String,
  resultType: String)