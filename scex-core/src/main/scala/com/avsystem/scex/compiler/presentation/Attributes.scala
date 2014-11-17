package com.avsystem.scex.compiler.presentation

/**
 * Author: ghik
 * Created: 11/17/14.
 */
class Attributes(
  val paramNames: Option[List[String]],
  val documentation: Option[String])

object Attributes {
  def apply(paramNames: List[String] = null, documentation: String = null) =
    new Attributes(Option(paramNames), Option(documentation))
}
