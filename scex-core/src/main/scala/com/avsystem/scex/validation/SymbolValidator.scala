package com.avsystem.scex.validation

import java.{util => ju, lang => jl}
import scala.reflect.runtime.{universe => ru}
import scala.language.implicitConversions
import scala.language.experimental.macros
import scala.reflect.macros.Context
import com.avsystem.scex.Utils._
import scala.reflect.api.Universe

object SymbolValidator {

  case class MemberAccessSpec(prefixTpe: Option[ru.Type], member: String, implicitConv: Option[String], allow: Boolean)

  def allow(expr: Any): List[SymbolValidator.MemberAccessSpec] = macro allow_impl

  def allow_impl(c: Context)(expr: c.Expr[Any]) =
    extractMemberAccessSpecs(c)(expr, allow = true)

  def deny(expr: Any): List[SymbolValidator.MemberAccessSpec] = macro deny_impl

  def deny_impl(c: Context)(expr: c.Expr[Any]) =
    extractMemberAccessSpecs(c)(expr, allow = false)

  private def extractMemberAccessSpecs(c: Context)(expr: c.Expr[Any], allow: Boolean) = {
    import c.universe._

    def accessSpec(prefix: Tree, body: Tree, implConv: Option[Tree]) = {
      val prefixTpeOpt = Some(prefix.tpe).filterNot(isJavaStaticType).map({ t: Type => t.map(_.widen) })
      (prefixTpeOpt, body.symbol, implConv.map(_.symbol))
    }

    def extractSymbols(body: Tree): List[(Option[Type], Symbol, Option[Symbol])] = {
      body match {
        case Select(apply@Apply(fun, List(prefix)), _) if isStaticImplicitConversion(fun.symbol) && apply.pos == prefix.pos =>
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
    }

    val rawAccessSpecs = extractSymbols(expr.tree)

    def reifyRuntimeType(tpe: Type) =
      reify(c.Expr[ru.TypeTag[_]](c.reifyType(treeBuild.mkRuntimeUniverseRef, EmptyTree, tpe)).splice.tpe)

    def reifySignature(symbol: Symbol) =
      c.literal(memberSignature(symbol))

    val reifiedAccessSpecs: List[Tree] = rawAccessSpecs.map {
      case (tpe, ms, ic) => reify(
        MemberAccessSpec(
          reifyOption(c)(tpe, reifyRuntimeType(_: Type)).splice,
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

class SymbolValidator {
  def isInvocationAllowed(u: Universe)(objType: u.Type, symbol: u.Symbol, implicitConv: u.Symbol): Boolean = false
}
