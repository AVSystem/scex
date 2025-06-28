package com.avsystem.scex

/**
 * Author: ghik
 * Created: 12/10/14.
 */
final case class Type(fullRepr: String, erasure: Class[_]) {
  override def toString: String = fullRepr
}

object Type {
  final val NoType = Type("NoType", null)
}
