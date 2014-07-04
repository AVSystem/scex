package com.avsystem.scex

import java.{lang => jl, util => ju}

/**
 * Created: 16-06-2014
 * Author: ghik
 */
class EvaluationException(message: String, cause: Throwable) extends RuntimeException(message, cause) {
  def this(code: String, line: Int, cause: Throwable) =
    this(s"Failed to evaluate expression - error in line $line:\n$code", cause)

  def this(cause: Throwable) =
    this(null, cause)
}

