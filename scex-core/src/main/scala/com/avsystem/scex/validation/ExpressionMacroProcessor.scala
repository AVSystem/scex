package com.avsystem.scex.validation

import java.{util => ju, lang => jl}
import scala.language.experimental.macros
import scala.reflect.macros.Context
import scala.util.DynamicVariable
import com.avsystem.scex.ExpressionProfile

/**
 * Object used during expression compilation to validate the expression (syntax, invocations, etc.)
 * This must be a Scala object and not a class because it contains macros. Validation is performed against
 * given ExpressionProfile which is injected into this object by ScexCompiler by means of a dynamic variable.
 */
object ExpressionMacroProcessor {

  val profileVar: DynamicVariable[ExpressionProfile] = new DynamicVariable(null)

  def processExpression[C, T](expr: T): T = macro processExpression_impl[C, T]

  def processExpression_impl[C: c.WeakTypeTag, T](c: Context)(expr: c.Expr[T]): c.Expr[T] = {
    import c.universe._
    val validationContext = ValidationContext(c.universe)(weakTypeOf[C])
    import validationContext._

    val profile = profileVar.value

    def isForbiddenThisReference(tree: Tree) = tree match {
      case tree: This if !tree.symbol.isPackage && !tree.symbol.isPackageClass => true
      case _ => false
    }

    expr.tree.foreach { subtree =>
      if (isForbiddenThisReference(subtree)) {
        c.error(subtree.pos, s"Cannot refer to 'this' or outer instances")
      }
      if (!profile.syntaxValidator.isSyntaxAllowed(c.universe)(subtree)) {
        c.error(subtree.pos, s"Cannot use language construct: ${subtree.getClass.getSimpleName}")
      }
    }

    val access = extractAccess(expr.tree)
    val validationResult = profile.symbolValidator.validateMemberAccess(validationContext)(access)

    validationResult.deniedAccesses.foreach { access =>
      c.error(access.pos, s"Member access forbidden: $access")
    }

    expr
  }
}
