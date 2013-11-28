package com.avsystem.scex

import java.{util => ju, lang => jl}

/**
 * Created: 28-11-2013
 * Author: ghik
 */
trait Setter[-T] extends (T => Unit)

// just to look nicer in Java
abstract class AbstractSetter[-T] extends Setter[T]
