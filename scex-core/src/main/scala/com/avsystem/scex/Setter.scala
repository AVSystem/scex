package com.avsystem.scex

import scala.annotation.implicitNotFound

/**
  * Created: 28-11-2013
  * Author: ghik
  */
trait Setter[-T] extends (T => Unit) {
  def acceptedType: Type
}

// just to look nicer in Java
abstract class AbstractSetter[-T] extends Setter[T]

@implicitNotFound("Property being set has type ${B}, but value has type ${A} and no conversion is available")
final case class SetterConversion[-A, +B](fun: A => B) extends AnyVal
object SetterConversion {
  def convert[A, B](a: A)(implicit sc: SetterConversion[A, B]): B = sc.fun(a)

  implicit def fallbackConversion[A]: SetterConversion[A, A] =
    SetterConversion(identity)
}
