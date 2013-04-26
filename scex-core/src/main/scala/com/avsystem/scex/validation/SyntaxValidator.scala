package com.avsystem.scex.validation

import java.{util => ju, lang => jl}
import reflect.macros.Universe
import scala.reflect.runtime.{universe => ru}

/**
 * Trait for expression syntax validator. This validator validates only language constructs, invocation validation
 * is performed by SymbolValidator.
 */
trait SyntaxValidator {
  def isSyntaxAllowed(u: Universe)(tree: u.Tree): Boolean
}

object SyntaxValidator {
  val SimpleExpressions = new SyntaxValidator {
    def isSyntaxAllowed(u: Universe)(tree: u.Tree): Boolean = {
      import u._

      tree match {
        case _: Block | _: Select | _: Apply | _: TypeApply | _: Ident |
             _: If | _: Literal | _: New | _: This | _: Typed | _: TypTree => true
        case _ => false
      }
    }
  }
}