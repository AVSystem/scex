package com.avsystem.scex
package util

import java.{lang => jl, util => ju}

/**
 * Created: 20-11-2013
 * Author: ghik
 */
object TypesafeEquals {

  import scala.language.experimental.macros

  implicit object TypesafeEqualsEnabled

  implicit class any2TripleEquals[A](left: A) {
    def ===[B](right: B): Boolean = macro Macros.tripleEquals_impl[A, B]
  }

}
