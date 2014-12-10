package com.avsystem.scex

/**
 * Author: ghik
 * Created: 12/10/14.
 */
case class Type(fullRepr: String, erasure: Class[_]) {
  override def toString = fullRepr
}
