package com.avsystem.scex.compiler

import java.{util => ju, lang => jl}
import scala.reflect.internal.util.BatchSourceFile

/**
 * Created: 13-12-2013
 * Author: ghik
 */
class ExpressionSourceFile(
  val exprDef: ExpressionDef,
  sourceName: String,
  code: String) extends BatchSourceFile(sourceName, code) {

  require(exprDef != null, "Expression definition cannot be null")
}
