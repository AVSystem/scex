package com.avsystem.scex
package validation

import java.{util => ju, lang => jl}
import reflect.macros.Universe


/**
 * Trait for expression syntax validator. This validator validates only language constructs, invocation validation
 * is performed by SymbolValidator.
 */
trait SyntaxValidator {
  def validateSyntax(u: Universe)(tree: u.Tree): (Boolean, List[u.Tree])
}

object SyntaxValidator {
  val SimpleExpressions: SyntaxValidator = new SyntaxValidator {
    def validateSyntax(u: Universe)(tree: u.Tree): (Boolean, List[u.Tree]) = {
      import u._

      def isLambdaParamDef(valDef: ValDef) =
        valDef.mods.hasFlag(Flag.PARAM) && valDef.rhs == EmptyTree

      tree match {
        case _: Block | _: Select | _: Apply | _: TypeApply | _: Ident |
             _: If | _: Literal | _: New | _: This | _: Typed | _: TypTree =>
          (true, tree.children)
        case Function(valDefs, body) if valDefs.forall(isLambdaParamDef) =>
          (true, body :: valDefs.map(_.tpt))
        case _ => (false, tree.children)
      }
    }
  }
}
