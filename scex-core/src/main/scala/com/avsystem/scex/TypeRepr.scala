package com.avsystem.scex

/**
  * Author: ghik
  * Created: 26/10/15.
  */
case class TypeRepr[T](repr: String)

object TypeRepr {

  import scala.reflect.runtime.{universe => ru}

  implicit def apply[T: ru.TypeTag]: TypeRepr[T] = macro Macros.materializeTypeRepr[T]
}