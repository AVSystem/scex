package com.avsystem.scex.util

import com.avsystem.scex.Macros
import java.{util => ju, lang => jl}

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

object Literal {

  import scala.language.experimental.macros

  implicit def literalToString(lit: Literal): String = lit.literalString

  implicit def literalToBoolean(lit: Literal): Boolean = macro Macros.literalToBoolean_impl

  implicit def literalToJBoolean(lit: Literal): jl.Boolean = macro Macros.literalToJBoolean_impl

  implicit def literalToChar(lit: Literal): Char = macro Macros.literalToChar_impl

  implicit def literalToJCharacter(lit: Literal): jl.Character = macro Macros.literalToJCharacter_impl

  implicit def literalToByte(lit: Literal): Byte = macro Macros.literalToByte_impl

  implicit def literalToJByte(lit: Literal): jl.Byte = macro Macros.literalToJByte_impl

  implicit def literalToShort(lit: Literal): Short = macro Macros.literalToShort_impl

  implicit def literalToJShort(lit: Literal): jl.Short = macro Macros.literalToJShort_impl

  implicit def literalToInt(lit: Literal): Int = macro Macros.literalToInt_impl

  implicit def literalToJInteger(lit: Literal): jl.Integer = macro Macros.literalToJInteger_impl

  implicit def literalToLong(lit: Literal): Long = macro Macros.literalToLong_impl

  implicit def literalToJLong(lit: Literal): jl.Long = macro Macros.literalToJLong_impl

  implicit def literalToFloat(lit: Literal): Float = macro Macros.literalToFloat_impl

  implicit def literalToJFloat(lit: Literal): jl.Float = macro Macros.literalToJFloat_impl

  implicit def literalToDouble(lit: Literal): Double = macro Macros.literalToDouble_impl

  implicit def literalToJDouble(lit: Literal): jl.Double = macro Macros.literalToJDouble_impl

}
