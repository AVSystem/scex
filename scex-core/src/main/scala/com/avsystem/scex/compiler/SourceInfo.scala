package com.avsystem.scex.compiler

import java.{util => ju, lang => jl}
import scala.collection.JavaConverters._
import scala.reflect.{ClassTag, classTag}

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