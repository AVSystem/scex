package com.avsystem.scex

import java.{lang => jl, util => ju}

import com.avsystem.scex.compiler.ExpressionDef

/**
 * Created: 16-06-2014
 * Author: ghik
 */
class ExpressionDebugInfo(
  val definition: ExpressionDef,
  val sourceInfo: SourceInfo)

class SourceInfo(
  val sourceName: String,
  val fullCode: String,
  val startOffset: Int,
  val endOffset: Int,
  val firstLine: Int,
  val lastLine: Int)
