package com.avsystem.scex.validation

import java.{util => ju, lang => jl}
import scala.reflect.runtime.{universe => ru}
import reflect.macros.Universe

/**
 * Trait for expression syntax validator. This validator validates only language constructs, invocation validation
 * is performed by SymbolValidator.
 */
trait SyntaxValidator {
  def isSyntaxAllowed(u: Universe)(tree: u.Tree): Boolean
}
