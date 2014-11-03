package com.avsystem.scex

import java.{lang => jl, util => ju}

/**
 * Created: 16-06-2014
 * Author: ghik
 */

import com.avsystem.scex.EvaluationException._

case class EvaluationException(lineWithNumber: Option[(String, Int)], cause: Throwable)
  extends RuntimeException(lineWithNumber.map((message _).tupled).orNull, cause) {

  def this(line: String, number: Int, cause: Throwable) =
    this(Some((line, number)), cause)

  def this(cause: Throwable) =
    this(None, cause)
}

object EvaluationException {
  def message(line: String, number: Int) =
    s"Failed to evaluate expression - error in line $number:\n$line"
}
