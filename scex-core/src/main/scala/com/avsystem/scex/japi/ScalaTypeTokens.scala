package com.avsystem.scex
package japi

import com.avsystem.scex.compiler.JavaTypeParsing
import com.google.common.reflect.TypeToken
import java.{util => ju, lang => jl}

/**
 * Created: 18-11-2013
 * Author: ghik
 */
object ScalaTypeTokens {
  def any = TypeToken.of(JavaTypeParsing.TypeAny).asInstanceOf[TypeToken[Any]]

  def anyVal = TypeToken.of(JavaTypeParsing.TypeAnyVal).asInstanceOf[TypeToken[AnyVal]]

  def anyRef = TypeToken.of(JavaTypeParsing.TypeAnyRef).asInstanceOf[TypeToken[AnyRef]]

  def scalaNull = TypeToken.of(JavaTypeParsing.TypeNull).asInstanceOf[TypeToken[Null]]

  def nothing = TypeToken.of(JavaTypeParsing.TypeNothing).asInstanceOf[TypeToken[Nothing]]
}
