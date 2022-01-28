package com.avsystem.scex.compiler

import com.avsystem.scex.{ExpressionContext, Macros}

case class ContextTypeInfo[C](fullTypeString: String, rootObjectClass: Class[_])
object ContextTypeInfo {
  // Scala, seriously, why do I have to do this with a macro?
  implicit def typeInfo[C <: ExpressionContext[_, _]]: ContextTypeInfo[C] = macro Macros.mkContextTypeInfo[C]
}
