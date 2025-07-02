package com.avsystem.scex

import scala.util.control.NoStackTrace

/**
  * Created: 16-06-2014
  * Author: ghik
  */
final case class EvaluationException(cause: Throwable) extends RuntimeException(cause) with NoStackTrace
