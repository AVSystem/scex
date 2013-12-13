package com.avsystem.scex.compiler

import java.{util => ju, lang => jl}
import com.avsystem.scex.ExpressionProfile
import scala.reflect.internal.util.BatchSourceFile

/**
 * Created: 13-12-2013
 * Author: ghik
 */
class ExpressionSourceFile(val profile: ExpressionProfile, sourceName: String, code: String)
  extends BatchSourceFile(sourceName, code) {

  require(profile != null, "Profile cannot be null")
}
