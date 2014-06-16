package com.avsystem.scex

import java.{lang => jl, util => ju}

/**
 * Created: 16-06-2014
 * Author: ghik
 */
class EvaluationException(val code: String, val line: Int, cause: Throwable)
  extends RuntimeException(s"in line $line:\n$code", cause)
