package com.avsystem.scex
package validation

import com.avsystem.scex.util.MacroUtils

import scala.annotation.nowarn
import scala.reflect.macros.Universe

abstract class ValidationContext protected extends MacroUtils {

  import universe._

  val contextTpe: Type

  private lazy val rootTpe = {
    val TypeRef(_, _, List(result, _)) = contextTpe.baseType(typeOf[ExpressionContext[_, _]].typeSymbol)
    result
  }

  sealed abstract class MemberAccess {
    def repr: String = repr("")

    private def repr(prefix: String): String = this match {
      case NoMemberAccess =>
        ""

      case SimpleMemberAccess(tpe, symbol, implicitConv, allowedByDefault, _) =>
        val implicitConvRepr = implicitConv match {
          case Some(tree) =>
            s" implicitly converted by |${path(tree)}|"
          case None => ""
        }

        s"$prefix|${memberSignature(symbol)}| on type |${tpe.widen}|$implicitConvRepr " +
          (if (allowedByDefault) "allowed" else "denied") + " by default"

      case MultipleMemberAccesses(accesses) =>
        val reprs = accesses.map(_.repr(prefix)).filter(_.nonEmpty)
        reprs.mkString("\n")

      case AlternativeMemberAccess(accesses) =>
        val reprs = accesses.map(_.repr(prefix + " ")).filter(_.nonEmpty)
        reprs.mkString(prefix + "--\n", "\n" + prefix + "OR\n", "\n" + prefix + "--")
    }
  }

  case object NoMemberAccess extends MemberAccess

  case class SimpleMemberAccess(
    tpe: Type,
    symbol: Symbol,
    implicitConv: Option[Tree],
    allowedByDefault: Boolean,
    pos: Position
  ) extends MemberAccess {

    override def toString: String =
      s"${memberSignature(symbol)} on type ${tpe.widen}" + implicitConv.map(ic => s" by $ic").getOrElse("")
  }

  // Multiple accesses, all must be allowed
  case class MultipleMemberAccesses(accesses: List[MemberAccess]) extends MemberAccess

  // An access that can be expressed in multiple ways - at least one of them must be allowed
  // before any other is deniedAccesses ("before" means earlier position in ACL)
  case class AlternativeMemberAccess(accesses: List[MemberAccess]) extends MemberAccess

  case class ValidationResult(priority: Int, deniedAccesses: List[SimpleMemberAccess])

  def toStringAccess(tree: Tree): SimpleMemberAccess =
    SimpleMemberAccess(tree.tpe, toStringSymbol(tree.tpe), None, allowedByDefault = false, tree.pos)

  def needsValidation(symbol: Symbol): Boolean =
    symbol != null && symbol.isTerm && !symbol.isPackage && !isExpressionUtil(symbol) && !isProfileObject(symbol)

  def hasNotValidatedAnnotation(symbol: Symbol): Boolean =
    symbol != null && annotationsIncludingOverrides(symbol).exists(_.tree.tpe <:< notValidatedAnnotType)

  @nowarn("msg=Recursive call used default arguments")
  def extractAccess(tree: Tree, allowedSelectionPrefix: Boolean = false): MemberAccess = tree match {
    case (_: Select | _: Ident) if hasNotValidatedAnnotation(tree.symbol) =>
      NoMemberAccess

    case Select(rootAdapter: Ident, _) if isRootAdapter(rootAdapter.tpe) && !isAdapterWrappedMember(tree.symbol) =>
      val symbol = getJavaGetter(tree.symbol, rootTpe)

      SimpleMemberAccess(rootTpe, symbol, None, allowedByDefault = false, tree.pos)

    case Select(apply@ImplicitlyConverted(qualifier, fun), _) if !isScexSynthetic(fun.symbol) =>
      val accessByImplicit = SimpleMemberAccess(qualifier.tpe, tree.symbol,
        Some(stripTypeApply(fun)), allowedByDefault = false, tree.pos)

      val implicitConversionAccess = extractAccess(fun, allowedSelectionPrefix = false)
      val plainAccess = SimpleMemberAccess(apply.tpe, tree.symbol, None, allowedByDefault = false, tree.pos)
      val alternatives = AlternativeMemberAccess(List(accessByImplicit, MultipleMemberAccesses(List(implicitConversionAccess, plainAccess))))

      // special case for configuration convenience: 'any + <string>' (using any2stringadd) is also validated as
      // combination of toString and string concatenation
      lazy val toStringMember = toStringSymbol(qualifier.tpe)
      val toStringAndConcatAccess =
        if (fun.symbol == any2stringadd && tree.symbol == stringAddPlus && toStringMember != NoSymbol) {
          val toStringAccess = SimpleMemberAccess(qualifier.tpe, toStringMember, None, allowedByDefault = false, tree.pos)
          val stringConcatAccess = SimpleMemberAccess(stringTpe, stringConcat, None, allowedByDefault = false, tree.pos)
          MultipleMemberAccesses(List(toStringAccess, stringConcatAccess))
        } else NoMemberAccess

      MultipleMemberAccesses(List(alternatives,
        toStringAndConcatAccess,
        extractAccess(qualifier, allowedSelectionPrefix = false)))

    case Select(apply@ImplicitlyConverted(qualifier, fun), _)
      if tree.symbol.isMethod && isAdapterConversion(fun.symbol) && !isAdapterWrappedMember(tree.symbol) =>

      val symbol = getJavaGetter(tree.symbol, qualifier.tpe)
      val access = SimpleMemberAccess(qualifier.tpe, symbol, None, allowedByDefault = false, tree.pos)

      MultipleMemberAccesses(List(access, extractAccess(qualifier, allowedSelectionPrefix = false)))

    case Select(qualifier, name) if needsValidation(tree.symbol) =>
      // Direct accesses to enum values are allowed because they compile to constants, so I'm also allowing all
      // Enum#valueOf methods for consistency.
      val staticMember = isStableStatic(qualifier.symbol) && !isFromToplevelType(tree.symbol)
      lazy val qualCompanion = qualifier.symbol.companion
      lazy val companionType = qualCompanion.asType.toType
      val enumValueOf = staticMember && name == TermName("valueOf") && qualCompanion != NoSymbol && companionType <:< typeOf[Enum[_]] &&
        tree.symbol.isMethod && tree.symbol.asMethod.paramLists.flatten.map(_.typeSignature).corresponds(List(typeOf[String]))(_ =:= _)

      val access = SimpleMemberAccess(qualifier.tpe, tree.symbol, None, allowedSelectionPrefix || enumValueOf, tree.pos)
      // When accessing member of static module (that includes Java statics), excluding getClass/equals/hashCode/toString/etc.
      // the static module access itself (qualifier) is allowed by default. Also, qualifier is allowed by default if its member was
      // allowed with @AlwaysAllowed annotation.
      MultipleMemberAccesses(List(access, extractAccess(qualifier, staticMember)))

    // special case for configuration convenience: string concatenation also forces validation of toString on its argument
    case Apply(qualifier, List(arg)) if qualifier.symbol == stringConcat =>
      MultipleMemberAccesses(List(toStringAccess(arg),
        extractAccess(qualifier, allowedSelectionPrefix = false),
        extractAccess(arg, allowedSelectionPrefix = false)))

    case Apply(fun, List(arg)) if fun.symbol == safeToString =>
      MultipleMemberAccesses(List(toStringAccess(arg), extractAccess(arg, allowedSelectionPrefix = false)))

    // special case for configuration convenience: standard string interpolations also force validation of
    // toString on its arguments
    case Apply(qualifier, args) if standardStringInterpolations contains qualifier.symbol =>
      val toStringAccesses = MultipleMemberAccesses(args.map(toStringAccess))
      MultipleMemberAccesses(toStringAccesses ::
        extractAccess(qualifier, allowedSelectionPrefix = false) ::
        args.map(arg => extractAccess(arg, allowedSelectionPrefix = false)))

    case _ =>
      MultipleMemberAccesses(tree.children.map(child => extractAccess(child, allowedSelectionPrefix = false)))
  }
}

object ValidationContext {
  def apply(u: Universe)(contextType: u.Type) = new ValidationContext {
    val universe: u.type = u
    val contextTpe = contextType
  }
}
