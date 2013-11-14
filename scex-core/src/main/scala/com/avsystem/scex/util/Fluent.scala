package com.avsystem.scex.util

import java.{util => ju, lang => jl}

/**
 * Created: 14-11-2013
 * Author: ghik
 */
trait Fluent {
  @inline
  protected def fluent(code: => Unit): this.type = {
    code
    this
  }
}
