package com.avsystem.scex

import scala.reflect.api.Universe

/**
 * Created with IntelliJ IDEA.
 * User: ghik
 * Date: 08.01.13
 * Time: 21:03
 */
object Utils {

  implicit class EnhancedInt(val i: Int) extends AnyVal {
    def times(expr: => Any) {
      var c = 0
      while (c < i) {
        expr
        c += 1
      }
    }
  }

  implicit class EnhancedString(val str: String) extends AnyVal {
    def leftPad(w: Int) =
      if (str.length >= w)
        str.substring(0, w)
      else {
        str + " " * (str.length - w)
      }
  }

  def benchmark(expr: => Any): Double = {
    val start = System.nanoTime()
    expr
    (System.nanoTime() - start) / 1000000000.0
  }

  def isModuleOrPackage(symbol: Universe#Symbol) = symbol != null &&
    (symbol.isModule || symbol.isModuleClass || symbol.isPackage || symbol.isPackageClass)

  def isJavaField(symbol: Universe#Symbol) =
    symbol != null && symbol.isJava && symbol.isTerm && !symbol.isMethod && !isModuleOrPackage(symbol)

  def memberSignature(s: Universe#Symbol) =
    if (s != null) s"${s.fullName}:${s.typeSignature}" else null

  def isStaticImplicitConversion(symbol: Universe#Symbol) =
    symbol != null && symbol.isMethod && symbol.isStatic && symbol.isImplicit
}
