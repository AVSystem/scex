package com.avsystem.scex.compiler

import java.{util => ju, lang => jl}

trait PackageGenerator {
  private var idx = 0

  protected def newPackageName(prefix: String) = {
    idx += 1
    prefix + "$" + idx
  }
}
