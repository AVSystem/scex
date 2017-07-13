package com.avsystem.scex
package util

/**
  * Created: 20-11-2013
  * Author: ghik
  */
object TypesafeEquals {
  implicit def typesafeEqualsEnabled: TypesafeEqualsEnabled = null
}

trait TypesafeEqualsEnabled
