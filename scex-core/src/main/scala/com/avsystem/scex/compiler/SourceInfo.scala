package com.avsystem.scex.compiler

/**
 * Created: 03-11-2014
 * Author: ghik
 */
class SourceInfo(
  val sourceName: String,
  val fullCode: String,
  val startOffset: Int,
  val endOffset: Int,
  val firstLine: Int,
  val lastLine: Int)