package com.avsystem.scex.compiler

import com.avsystem.scex.compiler.JavaTypeParsing._
import java.{util => ju, lang => jl}


object Manual {
  def main(args: Array[String]) {
    println(javaTypeAsScalaType(JavaTypes.complexParameterizedType()))
  }
}
