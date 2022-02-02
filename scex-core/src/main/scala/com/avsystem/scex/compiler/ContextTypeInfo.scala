package com.avsystem.scex.compiler

import com.avsystem.commons.annotation.bincompat
import com.avsystem.scex.{ExpressionContext, Macros}

case class ContextTypeInfo[C](fullTypeString: String, rootObjectClass: Class[_]) {
  @bincompat
  private[compiler] def this(fullTypeString: String, rootObjectClassName: String) =
    this(fullTypeString, Class.forName(rootObjectClassName))
}
object ContextTypeInfo {
  // Scala, seriously, why do I have to do this with a macro?
  implicit def typeInfo[C <: ExpressionContext[_, _]]: ContextTypeInfo[C] = macro Macros.mkContextTypeInfo[C]

  @bincompat
  private[compiler] def apply[C](fullTypeString: String, rootObjectClassName: String): ContextTypeInfo[C] =
    new ContextTypeInfo(fullTypeString, Class.forName(rootObjectClassName))
}
