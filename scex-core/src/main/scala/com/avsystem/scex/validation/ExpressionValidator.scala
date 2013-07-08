package com.avsystem.scex.validation

import com.avsystem.scex.compiler.ExpressionProfile
import java.{util => ju, lang => jl}
import scala.language.experimental.macros
import scala.reflect.macros.Context
import scala.util.DynamicVariable
import scala.runtime.StringAdd

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
    lazy val any2stringadd = typeOf[Predef.type].member(newTermName("any2stringadd"))
    lazy val stringAddPlus = typeOf[StringAdd].member(newTermName("+").encodedName)
    lazy val stringConcat = typeOf[String].member(newTermName("+").encodedName)
    lazy val stringTpe = typeOf[String]

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

    def toStringAccess(tree: Tree) =
      SimpleMemberAccess(tree.tpe, toStringSymbol(tree.tpe), None, allowedByDefault = false, tree.pos)

    def extractAccess(tree: Tree, staticAccess: Boolean = false): MemberAccess = {
      def needsValidation(symbol: Symbol) =
        symbol != null && symbol.isTerm && !symbol.isPackage && !isExpressionUtil(symbol) && !isProfileObject(symbol)

      tree match {
        case Select(contextAdapter@Ident(_), _) if isContextAdapter(contextAdapter.symbol) =>
          val symbol = getJavaGetter(tree.symbol, contextTpe)

          SimpleMemberAccess(contextTpe, symbol, None, allowedByDefault = false, tree.pos)

        case Select(apply@ImplicitlyConverted(qualifier, fun), _) if !isScexSynthetic(fun.symbol) =>
          val accessByImplicit = SimpleMemberAccess(qualifier.tpe, tree.symbol,
            Some(ImplicitConversion(fun, apply.tpe)), allowedByDefault = false, tree.pos)

          val implicitConversionAccess = extractAccess(fun)
          val plainAccess = SimpleMemberAccess(apply.tpe, tree.symbol, None, allowedByDefault = false, tree.pos)
          var alternatives = List(accessByImplicit, MultipleMemberAccesses(List(implicitConversionAccess, plainAccess)))

          // special case for configuration convenience: 'any + <string>' (using any2stringadd) is also validated as
          // combination of toString and string concatenation
          if (fun.symbol == any2stringadd && tree.symbol == stringAddPlus) {
            val toStringAccess = SimpleMemberAccess(qualifier.tpe, toStringSymbol(qualifier.tpe), None, allowedByDefault = false, tree.pos)
            val stringConcatAccess = SimpleMemberAccess(stringTpe, stringConcat, None, allowedByDefault = false, tree.pos)
            alternatives = MultipleMemberAccesses(List(toStringAccess, stringConcatAccess)) :: alternatives
          }

          MultipleMemberAccesses(List(AlternativeMemberAccess(alternatives), extractAccess(qualifier)))

        case Select(apply@ImplicitlyConverted(qualifier, fun), _) if isAdapter(apply.tpe) =>
          val symbol = getJavaGetter(tree.symbol, qualifier.tpe)
          val access = SimpleMemberAccess(qualifier.tpe, symbol, None, allowedByDefault = false, tree.pos)

          MultipleMemberAccesses(List(access, extractAccess(qualifier)))

        case Select(qualifier, _) if needsValidation(tree.symbol) =>
          val access = SimpleMemberAccess(qualifier.tpe, tree.symbol, None, staticAccess, tree.pos)
          // When accessing member of static module (that includes Java statics) not from Any/AnyVal/AnyRef
          // the static module access itself is allowed by default.
          val staticMember = isStaticModule(qualifier.symbol) && !isFromToplevelType(tree.symbol)
          MultipleMemberAccesses(List(access, extractAccess(qualifier, staticMember)))

        // special case for configuration convenience: string concatenation also forces validation of toString on its argument
        case Apply(qualifier, List(arg)) if qualifier.symbol == stringConcat =>
          MultipleMemberAccesses(List(toStringAccess(arg), extractAccess(qualifier), extractAccess(arg)))

        // special case for configuration convenience: standard string interpolations also force validation of
        // toString on its arguments
        case Apply(qualifier, args) if standardStringInterpolations contains qualifier.symbol =>
          val toStringAccesses = MultipleMemberAccesses(args.map(toStringAccess))
          MultipleMemberAccesses(toStringAccesses :: extractAccess(qualifier) :: args.map(arg => extractAccess(arg)))

        case _ =>
          MultipleMemberAccesses(tree.children.map(child => extractAccess(child)))
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
