package com.avsystem.scex
package util

/**
  * Created: 14-11-2013
  * Author: ghik
  */
trait Fluent {
  @inline
  protected final def fluent(code: Unit): this.type = this
}
