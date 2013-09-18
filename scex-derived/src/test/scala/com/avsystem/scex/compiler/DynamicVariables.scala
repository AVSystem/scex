package com.avsystem.scex.compiler

import com.avsystem.scex.util.DynamicAdapters.DynamicVariableSupport
import java.{util => ju, lang => jl}
import scala.collection.mutable

/**
 * Created: 18-09-2013
 * Author: ghik
 */
class DynamicVariables extends DynamicVariableSupport[String] {
  private val map = new mutable.HashMap[String, String]

  def set(name: String, value: String) = updateDynamic(name)(value)

  def get(name: String) = selectDynamic(name)

  def selectDynamic(name: String) = map(name)

  def updateDynamic(name: String)(value: String) {
    map(name) = value
  }
}
