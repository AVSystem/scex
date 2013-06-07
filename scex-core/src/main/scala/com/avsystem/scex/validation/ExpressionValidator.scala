package com.avsystem.scex.validation

import com.avsystem.scex.compiler.ExpressionProfile
import com.avsystem.scex.compiler.annotation._
import java.{util => ju, lang => jl}
import scala.language.experimental.macros
import scala.reflect.macros.Context
import scala.util.DynamicVariable

/**
 * Object used during expression compilation to validate the expression (syntax, invocations, etc.)
 * This must be a Scala object and not a class because it contains macros. Validation is performed against
 * given ExpressionProfile which is injected into this object by ScexCompiler by means of a dynamic variable.
 */
object ExpressionValidator {

  val profileVar: DynamicVariable[ExpressionProfile] = new DynamicVariable(null)

  def validate[C, R](expr: R): R = macro validate_impl[C, R]

  def validate_impl[C: c.WeakTypeTag, R](c: Context)(expr: c.Expr[R]): c.Expr[R] = {
    import c.universe._
    val validationContext = ValidationContext(c)
    import validationContext.{c => _, _}

    lazy val contextTpe = weakTypeOf[C]

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

    def extractAccess(tree: Tree, staticAccess: Boolean): MemberAccess = {
      def needsValidation(symbol: Symbol) =
        symbol != null && symbol.isTerm && !symbol.isPackage && !isExpressionUtil(symbol) && !isProfileObject(symbol)

      tree match {
        case Select(contextAdapter@Ident(_), _) if isContextAdapter(contextAdapter.symbol) =>
          val symbol = getJavaGetter(tree.symbol, contextTpe)

          SimpleMemberAccess(contextTpe, symbol, None, allowedByDefault = false, tree.pos)

        case Select(apply@ImplicitlyConverted(qualifier, fun), _) if !isScexSynthetic(fun.symbol) =>
          val accessByImplicit = SimpleMemberAccess(qualifier.tpe, tree.symbol,
            Some(ImplicitConversion(fun, apply.tpe)), allowedByDefault = false, tree.pos)

          val implicitConversionAccess = extractAccess(fun, staticAccess = false)
          val plainAccess = SimpleMemberAccess(apply.tpe, tree.symbol, None, allowedByDefault = false, tree.pos)
          val access = AlternativeMemberAccess(List(accessByImplicit, MultipleMemberAccesses(List(implicitConversionAccess, plainAccess))))

          MultipleMemberAccesses(List(access, extractAccess(qualifier, staticAccess = false)))

        case Select(apply@ImplicitlyConverted(qualifier, fun), _) if isAdapter(apply.tpe) =>
          val symbol = getJavaGetter(tree.symbol, qualifier.tpe)
          val access = SimpleMemberAccess(qualifier.tpe, symbol, None, allowedByDefault = false, tree.pos)

          MultipleMemberAccesses(List(access, extractAccess(qualifier, staticAccess = false)))

        case Select(qualifier, _) if needsValidation(tree.symbol) =>
          val access = SimpleMemberAccess(qualifier.tpe, tree.symbol, None, staticAccess, tree.pos)
          // When accessing member of static module (that includes Java statics) not from Any/AnyVal/AnyRef
          // the static module access itself is allowed by default.
          val staticMember = isStaticModule(qualifier.symbol) && !isFromToplevelType(tree.symbol)
          MultipleMemberAccesses(List(access, extractAccess(qualifier, staticMember)))

        case _ =>
          MultipleMemberAccesses(tree.children.map(child => extractAccess(child, staticAccess = false)))
      }
    }

    val access = extractAccess(expr.tree, staticAccess = false)
    println(s"VALIDATING\n${access.repr}")
    val validationResult = profile.symbolValidator.isMemberAccessAllowed(validationContext)(access)

    validationResult.deniedAccesses.foreach { access =>
      c.error(access.pos, s"Member access forbidden: $access")
    }

    expr
  }
}
