package com.avsystem.scex
package util

import java.{util => ju, lang => jl}

/**
 * Created: 18-11-2013
 * Author: ghik
 */
case class Literal(literalString: String) {
  override def toString = literalString

  def toBoolean = literalString.toBoolean

  def toChar =
    if (literalString.length == 1) literalString.charAt(0)
    else throw new IllegalArgumentException(s"Expected string with exactly one character, got ${'"'}$literalString${'"'}")

  def toByte = literalString.toByte

  def toShort = literalString.toShort

  def toInt = literalString.toInt

  def toLong = literalString.toLong

  def toFloat = literalString.toFloat

  def toDouble = literalString.toDouble
}

object Literal {

  implicit def literalToString(lit: Literal): String =
    lit.literalString

  implicit def literalToBoolean(lit: Literal): Boolean =
    lit.toBoolean

  implicit def literalToJBoolean(lit: Literal): jl.Boolean =
    lit.toBoolean

  implicit def literalToChar(lit: Literal): Char =
    lit.toChar

  implicit def literalToJCharacter(lit: Literal): jl.Character =
    lit.toChar

  implicit def literalToByte(lit: Literal): Byte =
    lit.toByte

  implicit def literalToJByte(lit: Literal): jl.Byte =
    lit.toByte

  implicit def literalToShort(lit: Literal): Short =
    lit.toShort

  implicit def literalToJShort(lit: Literal): jl.Short =
    lit.toShort

  implicit def literalToInt(lit: Literal): Int =
    lit.toInt

  implicit def literalToJInteger(lit: Literal): jl.Integer =
    lit.toInt

  implicit def literalToLong(lit: Literal): Long =
    lit.toLong

  implicit def literalToJLong(lit: Literal): jl.Long =
    lit.toLong

  implicit def literalToFloat(lit: Literal): Float =
    lit.toFloat

  implicit def literalToJFloat(lit: Literal): jl.Float =
    lit.toFloat

  implicit def literalToDouble(lit: Literal): Double =
    lit.toDouble

  implicit def literalToJDouble(lit: Literal): jl.Double =
    lit.toDouble

}
