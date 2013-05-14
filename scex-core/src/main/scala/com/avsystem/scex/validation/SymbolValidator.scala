package com.avsystem.scex.validation

import SymbolValidator._
import com.avsystem.scex.util.CommonUtils._
import java.{util => ju, lang => jl}
import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.reflect.api.{Universe, TypeCreator}
import scala.reflect.macros.Context

class SymbolValidator(accessSpecs: List[MemberAccessSpec]) {

  def isInvocationAllowed(c: Context)
    (objType: c.universe.Type, invocationSymbol: c.universe.Symbol, invocationConvOpt: Option[c.universe.Symbol]): Boolean = {

    import c.universe._

    val invocationSymbolSignatures =
      (invocationSymbol :: invocationSymbol.allOverriddenSymbols).view.map(memberSignature).toSet

    def symbolsMatch(specSymbol: String, actualSymbol: Symbol) =
      invocationSymbolSignatures.contains(specSymbol)

    val invocationConvSignatureOpt = invocationConvOpt.map(memberSignature)

    accessSpecs.collectFirst {
      case MemberAccessSpec(typeCreator, specSymbol, specConvOpt, allow)
        if specConvOpt == invocationConvSignatureOpt &&
          symbolsMatch(specSymbol, invocationSymbol) &&
          objType <:< typeCreator.typeIn(c.universe) =>
        allow
    } getOrElse (false)
  }

  lazy val referencedJavaClasses = accessSpecs.collect({
    case MemberAccessSpec(typeInfo, _, _, true)
      if typeInfo.clazz.isDefined && typeInfo.isJava =>
      hierarchy(typeInfo.clazz.get)
  }).flatten.toSet
}

object SymbolValidator {

  case class MemberAccessSpec(typeInfo: TypeInfo, member: String, implicitConv: Option[String], allow: Boolean)

  implicit class WildcardMemberAccess(val wrapped: Any) extends AnyVal {
    private def notImplemented: List[MemberAccessSpec] =
      throw new NotImplementedError("You cannot call this method outside of symbol validator DSL")

    /**
     * Allows for accessing or calling any member (field, value, variable, method) with given name
     */
    def anyNamed(name: String) = notImplemented

    /**
     * Allows for creating new instances of given type using any public constructor.
     * @return
     */
    def anyConstructor = notImplemented
  }

  def allow(expr: Any): List[SymbolValidator.MemberAccessSpec] = macro allow_impl

  def allow_impl(c: Context)(expr: c.Expr[Any]) =
    extractMemberAccessSpecs(c)(expr, allow = true)

  def deny(expr: Any): List[SymbolValidator.MemberAccessSpec] = macro deny_impl

  def deny_impl(c: Context)(expr: c.Expr[Any]) =
    extractMemberAccessSpecs(c)(expr, allow = false)

  private def extractMemberAccessSpecs(c: Context)(expr: c.Expr[Any], allow: Boolean): c.Expr[List[MemberAccessSpec]] = {
    import c.universe._

    object ImplicitlyConverted {
      def unapply(tree: Tree) = tree match {
        case Apply(fun, List(prefix)) if isStaticImplicitConversion(fun.symbol) && tree.pos == prefix.pos =>
          Some((prefix, fun))
        case _ =>
          None
      }
    }

    def accessSpec(prefix: Tree, body: Tree, implConv: Option[Tree]) = {
      (prefix.tpe, body.symbol, implConv.map(_.symbol))
    }

    def allMemberSpecsNamed(tpe: Type, name: Name) = {
      val member = tpe.member(name)
      if (member.isTerm) {
        Some(member.asTerm.alternatives.map(symbol => (tpe, symbol, None)))
      } else {
        None
      }
    }

    def extractSymbols(body: Tree): List[(Type, Symbol, Option[Symbol])] = body match {
      case Apply(Select(implConv@ImplicitlyConverted(prefix, fun), termName), List(memberNameLiteral@Literal(Constant(memberName: String))))
        if termName == newTermName("anyNamed") && implConv.tpe <:< typeOf[WildcardMemberAccess] =>

        allMemberSpecsNamed(prefix.tpe, newTermName(memberName).encodedName).getOrElse {
          c.error(memberNameLiteral.pos, s"${prefix.tpe.map(_.widen)} has no members named $memberName")
          Nil
        }

      case Select(implConv@ImplicitlyConverted(prefix, fun), termName)
        if termName == newTermName("anyConstructor") && implConv.tpe <:< typeOf[WildcardMemberAccess] =>

        allMemberSpecsNamed(prefix.tpe, nme.CONSTRUCTOR).getOrElse {
          c.error(body.pos, s"${prefix.tpe.map(_.widen)} has no constructors")
          Nil
        }

      case Select(ImplicitlyConverted(prefix, fun)) =>
        List(accessSpec(prefix, body, Some(fun)))

      case Select(prefix, _) =>
        List(accessSpec(prefix, body, None))

      case Apply(inner, _) =>
        extractSymbols(inner)

      case TypeApply(inner, _) =>
        extractSymbols(inner)

      case Function(_, actualBody) =>
        extractSymbols(actualBody)

      case Block(stats, finalExpr) =>
        (finalExpr :: stats).flatMap(extractSymbols(_: Tree))

      case _ =>
        c.error(body.pos, "Bad symbol specification syntax: ")
        Nil
    }

    val rawAccessSpecs = extractSymbols(expr.tree)

    def reifySignature(symbol: Symbol) =
      c.literal(memberSignature(symbol))

    def reifyTypeCreator(tpe: Type) = {
      val widenedTpe = tpe.map(_.widen)

      val Block(List(_, _), Apply(_, List(_, typeCreatorTree))) =
        c.reifyType(treeBuild.mkRuntimeUniverseRef, EmptyTree, widenedTpe)

      reify(new TypeInfo(
        c.Expr[TypeCreator](typeCreatorTree).splice,
        reifyRuntimeClassOpt(c)(tpe).splice,
        c.literal(tpe.typeSymbol.isJava).splice,
        c.literal(widenedTpe.toString).splice))
    }

    val reifiedAccessSpecs: List[Tree] = rawAccessSpecs.map {
      case (tpe, ms, ic) => reify(
        MemberAccessSpec(
          reifyTypeCreator(tpe).splice,
          reifySignature(ms).splice,
          reifyOption(c)(ic, reifySignature(_: Symbol)).splice,
          c.literal(allow).splice))
        .tree
    }

    val typesAndMethodsListExpr: c.Expr[List[MemberAccessSpec]] =
      c.Expr(Apply(Select(reify(List).tree, newTermName("apply")), reifiedAccessSpecs))

    typesAndMethodsListExpr
  }
}
