package com.avsystem.scex.presentation

/**
 * Author: ghik
 * Created: 11/17/14.
 */
final class Attributes(
  val paramNames: Option[List[String]],
  val documentation: Option[String]) {

  def orElse(attrs: => Attributes): Attributes = {
    lazy val otherAttrs = attrs
    new Attributes(
      paramNames orElse otherAttrs.paramNames,
      documentation orElse otherAttrs.documentation
    )
  }

}

object Attributes {
  def apply(paramNames: List[String] = null, documentation: String = null) =
    new Attributes(Option(paramNames), Option(documentation))

  val empty = Attributes()
}
