package com.avsystem.scex.compiler

import com.avsystem.scex.{ExpressionContext, Macros}

case class ContextTypeInfo[C](fullTypeString: String, rootObjectClassName: String) {
  def resolveRootClass(): Class[_] = rootObjectClassName match {
    case "void" => classOf[Unit]
    case "boolean" => classOf[Boolean]
    case "char" => classOf[Char]
    case "byte" => classOf[Byte]
    case "short" => classOf[Short]
    case "int" => classOf[Int]
    case "long" => classOf[Long]
    case "float" => classOf[Float]
    case "double" => classOf[Double]
    case other => Class.forName(other)
  }
}
object ContextTypeInfo {
  // Scala, seriously, why do I have to do this with a macro?
  implicit def typeInfo[C <: ExpressionContext[_, _]]: ContextTypeInfo[C] = macro Macros.mkContextTypeInfo[C]
}
