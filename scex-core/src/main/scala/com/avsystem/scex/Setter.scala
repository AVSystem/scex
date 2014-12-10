package com.avsystem.scex

import java.{lang => jl, util => ju}

/**
 * Created: 28-11-2013
 * Author: ghik
 */
trait Setter[-T] extends (T => Unit) {
  def acceptedType: Type
}

// just to look nicer in Java
abstract class AbstractSetter[-T] extends Setter[T]
