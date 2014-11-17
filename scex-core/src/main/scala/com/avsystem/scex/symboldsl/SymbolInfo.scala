package com.avsystem.scex
package symboldsl

/**
 * Author: ghik
 * Created: 11/14/14.
 */
case class SymbolInfo[T](typeInfo: TypeInfo, memberSignature: String, implicitConv: Option[String], payload: T)
