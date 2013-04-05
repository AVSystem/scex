package com.avsystem.scex.validation

import java.{util => ju, lang => jl}
import scala.reflect.runtime.{universe => ru}
import reflect.macros.Universe

trait SyntaxValidator {
  def isSyntaxAllowed(u: Universe)(tree: u.Tree): Boolean
}
