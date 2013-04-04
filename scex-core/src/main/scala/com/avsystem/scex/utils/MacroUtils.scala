package com.avsystem.scex.utils

import java.{util => ju, lang => jl}
import scala.collection.mutable
import scala.collection.JavaConversions._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.api.Universe

object MacroUtils {
  def isModuleOrPackage(symbol: Universe#Symbol) = symbol != null &&
    (symbol.isModule || symbol.isModuleClass || symbol.isPackage || symbol.isPackageClass)

  def isJavaField(symbol: Universe#Symbol) =
    symbol != null && symbol.isJava && symbol.isTerm && !symbol.isMethod && !isModuleOrPackage(symbol)

  def memberSignature(s: Universe#Symbol) = s"${s.fullName}:${s.typeSignature}"

  def isStaticImplicitConversion(symbol: Universe#Symbol) =
    symbol != null && symbol.isMethod && symbol.isStatic && symbol.isImplicit
}
