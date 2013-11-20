package com.avsystem.scex.compiler

import java.{util => ju, lang => jl}

/**
 * Created: 20-11-2013
 * Author: ghik
 */
object EqualsPlayground {
  def main(args: Array[String]) {
    import com.avsystem.scex.util.TypesafeEquals._

    implicit def intToStr(int: Int) = int.toString
    implicit def strToInt(str: String) = str.toInt

    println(1 === "1")
    println("1" === 1)
  }
}
