package com.avsystem.scex
package compiler

import java.{lang => jl, util => ju}

import com.avsystem.scex.compiler.TemplateInterpolations.Splicer

/**
 * Created: 31-03-2014
 * Author: ghik
 */
class FancySplicedRoot {
  def self = this
}

object FancySplicedRoot {

  implicit object fancySplicer extends Splicer[FancySplicedRoot] {
    def toString(t: FancySplicedRoot) = "FANCY"
  }

}
