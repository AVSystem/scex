package com.avsystem.scex.compiler

import java.{util => ju, lang => jl}
import scala.collection.mutable
import scala.collection.JavaConverters._
import scala.reflect.runtime.{universe => ru}
import com.avsystem.scex.compiler.JavaTypeParsing._
import com.avsystem.scex.compiler.ParameterizedClass.StaticInnerGeneric

object Manual {
  def main(args: Array[String]) {
    println(javaTypeAsScalaType(JavaTypes.complexParameterizedType()))
  }
}
