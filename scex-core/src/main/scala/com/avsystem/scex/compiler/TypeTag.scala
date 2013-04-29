package com.avsystem.scex.compiler

import java.lang.reflect.{Type, ParameterizedType}
import java.{util => ju, lang => jl}
import scala.reflect.runtime.{universe => ru}

abstract class TypeTag[T] extends Type {
  require(getClass.getSuperclass == classOf[TypeTag[_]], "You must directly extend TypeTag")

  val underlyingType =
    getClass.getGenericSuperclass.asInstanceOf[ParameterizedType].getActualTypeArguments.apply(0)

}

object TypeTag {
  def unapply(tag: TypeTag[_]) = Some(tag.underlyingType)
}
