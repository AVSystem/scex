package com.avsystem.scex

import java.{util => ju, lang => jl}
import scala.collection.mutable
import scala.collection.JavaConverters._
import scala.reflect.runtime.{universe => ru}
import java.lang.reflect.{Type, ParameterizedType}

abstract class TypeTag[T] extends Type {
  require(getClass.getSuperclass == classOf[TypeTag[_]], "You must directly extend TypeTag")

  val underlyingType =
    getClass.getGenericSuperclass.asInstanceOf[ParameterizedType].getActualTypeArguments.apply(0)

}

object TypeTag {
  def unapply(tag: TypeTag[_]) = Some(tag.underlyingType)
}
