package com.avsystem.scex
package util

import java.{util => ju, lang => jl}

/**
 * Created: 14-11-2013
 * Author: ghik
 */
trait Fluent {
  @inline
  protected final def fluent(code: => Unit): this.type = {
    code
    this
  }
}
