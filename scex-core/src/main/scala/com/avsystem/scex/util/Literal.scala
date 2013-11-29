package com.avsystem.scex
package util

import java.{util => ju, lang => jl}
import scala.collection.immutable.{WrappedString, StringOps}

/**
 * Created: 18-11-2013
 * Author: ghik
 */
case class Literal(literalString: String) {
  override def toString = literalString

  def toBoolean = literalString.trim.toLowerCase match {
    case "true" | "1" => true
    case "false" | "0" => false
    case _ => throw new IllegalArgumentException
  }

  def toChar = literalString.trim match {
    case charString if charString.length == 1 => charString.charAt(0)
    case _ => throw new IllegalArgumentException
  }

  def toByte = jl.Byte.valueOf(literalString.trim)

  def toShort = jl.Short.valueOf(literalString.trim)

  def toInt = jl.Integer.valueOf(literalString.trim)

  def toLong = jl.Long.valueOf(literalString.trim)

  def toFloat = jl.Float.valueOf(literalString.trim)

  def toDouble = jl.Double.valueOf(literalString.trim)

  def +(other: Any) = literalString + other.toString
}

trait LowPriorityImplicits {
  implicit def literalToBoolean(lit: Literal): Boolean = lit.toBoolean

  implicit def literalToJBoolean(lit: Literal): jl.Boolean = lit.toBoolean

  implicit def literalToChar(lit: Literal): Char = lit.toChar

  implicit def literalToJCharacter(lit: Literal): jl.Character = lit.toChar

  implicit def literalToByte(lit: Literal): Byte = lit.toByte

  implicit def literalToJByte(lit: Literal): jl.Byte = lit.toByte

  implicit def literalToShort(lit: Literal): Short = lit.toShort

  implicit def literalToJShort(lit: Literal): jl.Short = lit.toShort

  implicit def literalToInt(lit: Literal): Int = lit.toInt

  implicit def literalToJInteger(lit: Literal): jl.Integer = lit.toInt

  implicit def literalToLong(lit: Literal): Long = lit.toLong

  implicit def literalToJLong(lit: Literal): jl.Long = lit.toLong

  implicit def literalToFloat(lit: Literal): Float = lit.toFloat

  implicit def literalToJFloat(lit: Literal): jl.Float = lit.toFloat

  implicit def literalToDouble(lit: Literal): Double = lit.toDouble

  implicit def literalToJDouble(lit: Literal): jl.Double = lit.toDouble
}

object Literal extends LowPriorityImplicits {

  implicit def literalToString(lit: Literal): String = lit.literalString

  implicit def literalToStringOps(lit: Literal): StringOps = lit.literalString

  implicit def literalToWrappedString(lit: Literal): WrappedString = lit.literalString

}
