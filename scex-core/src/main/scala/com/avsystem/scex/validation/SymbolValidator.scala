package com.avsystem.scex.validation

import java.{util => ju, lang => jl}
import scala.language.implicitConversions
import scala.language.experimental.macros
import scala.reflect.macros.Context
import com.avsystem.scex.Utils._
import scala.tools.reflect.ReflectGlobal
import scala.reflect.api.{Universe, JavaMirrors}

object SymbolValidator {

  case class MemberAccessSpec(clazzOpt: Option[Class[_]], member: String, implicitConv: Option[String], allow: Boolean)

  def allow(expr: Any): List[SymbolValidator.MemberAccessSpec] = macro allow_impl

  def allow_impl(c: Context)(expr: c.Expr[Any]) =
    extractMemberAccessSpecs(c)(expr, allow = true)

  def deny(expr: Any): List[SymbolValidator.MemberAccessSpec] = macro deny_impl

  def deny_impl(c: Context)(expr: c.Expr[Any]) =
    extractMemberAccessSpecs(c)(expr, allow = false)

  private def extractMemberAccessSpecs(c: Context)(expr: c.Expr[Any], allow: Boolean) = {
    import c.universe._

    def accessSpec(prefix: Tree, body: Tree, implConv: Option[Tree]) = {
      (prefix.tpe, body.symbol, implConv.map(_.symbol))
    }

    def extractSymbols(body: Tree): List[(Type, Symbol, Option[Symbol])] = body match {
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

    val rawAccessSpecs = extractSymbols(expr.tree)

    def reifySignature(symbol: Symbol) =
      c.literal(memberSignature(symbol))

    val reifiedAccessSpecs: List[Tree] = rawAccessSpecs.map {
      case (tpe, ms, ic) => reify(
        MemberAccessSpec(
          reifyRuntimeClassOpt(c)(tpe).splice,
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

import SymbolValidator._

class SymbolValidator(accessSpecs: List[MemberAccessSpec]) {

  def isInvocationAllowed(global: Universe with JavaMirrors, c: Context)
    (objType: c.universe.Type, invocationSymbol: c.universe.Symbol, invocationConvOpt: Option[c.universe.Symbol]): Boolean = {

    import c.universe._

    val importer = global.mkImporter(c.universe).asInstanceOf[global.Importer {val from: c.universe.type}]
    val runtimeMirror = global.runtimeMirror(this.getClass.getClassLoader)

    def runtimeClass(tpe: Type) =
      runtimeMirror.runtimeClass(importer.importSymbol(objType.typeSymbol).asClass)

    val actualClassOpt: Option[Class[_]] =
      if (isJavaStaticType(objType))
        None
      else
        Some(runtimeClass(objType))

    def classesMatch(clazzOpt: Option[Class[_]]) =
      (clazzOpt, actualClassOpt) match {
        case (None, None) => true
        case (Some(clazz), Some(actualClazz)) => clazz.isAssignableFrom(actualClazz)
        case _ => false
      }

    val invocationSymbolSignatures =
      (invocationSymbol :: invocationSymbol.allOverriddenSymbols).view.map(memberSignature).toSet

    def symbolsMatch(specSymbol: String, actualSymbol: Symbol) =
      invocationSymbolSignatures.contains(specSymbol)

    val invocationConvSignatureOpt = invocationConvOpt.map(memberSignature)

    accessSpecs.collectFirst {
      case MemberAccessSpec(clazzOpt, specSymbol, specConvOpt, allow) if specConvOpt == invocationConvSignatureOpt &&
        symbolsMatch(specSymbol, invocationSymbol) && classesMatch(clazzOpt) => allow
    } getOrElse (false)
  }

  lazy val referencedClasses =
    accessSpecs.collect({
      case MemberAccessSpec(Some(clazz), _, _, true) => clazz
    }).toSet.toList
}
