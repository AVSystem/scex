package com.avsystem.scex
package compiler

import java.{util => ju, lang => jl}
import com.avsystem.scex.util.Literal

/**
 * Created: 04-04-2014
 * Author: ghik
 */
object TestUtils {
  implicit def zeroOneLiteralToBoolean(lit: Literal) = lit.literalString match {
    case "0" => false
    case "1" => true
    case _ => throw new IllegalArgumentException(s"Must be 0 or 1, found ${lit.literalString}")
  }

  class CustomBooleanConversionRoot(falseVal: String, trueVal: String) {
    implicit def customLiteralToBoolean(lit: Literal) = lit.literalString match {
      case `falseVal` => false
      case `trueVal` => true
      case _ => throw new IllegalArgumentException(s"Must be $falseVal or $trueVal, found ${lit.literalString}")
    }
  }

}
