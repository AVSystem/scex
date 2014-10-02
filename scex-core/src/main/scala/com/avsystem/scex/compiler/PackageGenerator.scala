package com.avsystem.scex
package compiler

import java.util.concurrent.atomic.AtomicInteger
import java.{util => ju, lang => jl}

trait PackageGenerator {
  private val idx = new AtomicInteger(0)

  protected def newPackageName(prefix: String) =
    prefix + "$" + idx.incrementAndGet()
}
