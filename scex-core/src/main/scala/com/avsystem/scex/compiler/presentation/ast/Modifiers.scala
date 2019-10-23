package com.avsystem.scex
package compiler.presentation.ast

import com.avsystem.commons.jiop.JavaInterop._

/**
 * Created: 12-03-2014
 * Author: ghik
 */
case class Modifiers(flags: Flags, privateWithin: Name, annotations: List[Tree]) extends PrettyPrint {
  def annotationsAsJava = annotations.asJava
}

object Modifiers {
  final val empty = Modifiers(Flags.empty, TypeName.EMPTY, Nil)
}