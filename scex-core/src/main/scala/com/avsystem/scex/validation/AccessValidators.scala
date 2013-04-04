package com.avsystem.scex.validation

import scala.language.experimental.macros
import java.io.{PrintWriter, FileWriter}
import scala.reflect.macros.Context
import scala.reflect.runtime.{universe => ru}
import com.avsystem.scex.utils.MacroUtils._

object AccessValidators {

  /* Allow or deny everything */

  def allowAny: AccessValidator =
    new AlwaysMatchingValidator(allow = true)

  def denyAny: AccessValidator =
    new AlwaysMatchingValidator(allow = false)

  /* Allow or deny Scala package and module (object) members and Java static members */

  def allow(expr: Any): AccessValidator = macro allow_impl

  def allow_impl(c: Context)(expr: c.Expr[Any]) =
    reifyTypeMembersValidator(c)(None, expr.tree, allow = true)

  def deny(expr: Any): AccessValidator = macro deny_impl

  def deny_impl(c: Context)(expr: c.Expr[Any]) =
    reifyTypeMembersValidator(c)(None, expr.tree, allow = false)

  /* Allow or deny members on instances of some concrete type */

  def allowOn[T](expr: T => Any): AccessValidator = macro allowOn_impl[T]

  def allowOn_impl[T: c.WeakTypeTag](c: Context)(expr: c.Expr[T => Any]): c.Expr[AccessValidator] =
    allowOrDenyOnImpl(c)(expr, allow = true)

  def denyOn[T](expr: T => Any): AccessValidator = macro denyOn_impl[T]

  def denyOn_impl[T: c.WeakTypeTag](c: Context)(expr: c.Expr[T => Any]): c.Expr[AccessValidator] =
    allowOrDenyOnImpl(c)(expr, allow = false)

  private def allowOrDenyOnImpl[T: c.WeakTypeTag](c: Context)(expr: c.Expr[T => Any], allow: Boolean): c.Expr[AccessValidator] = {
    import c.universe._
    expr.tree match {
      case Function(List(ValDef(_, _, TypeTree(), EmptyTree)), body) =>
        reifyTypeMembersValidator(c)(Some(weakTypeOf[T]), body, allow)
      case _ =>
        c.error(expr.tree.pos, "Bad symbol specification syntax - expected function.")
        null
    }
  }

  /*
  The big macro that extracts classes and members from expressions passed to allow/deny methods
   */
  private def reifyTypeMembersValidator(c: Context)(requiredPrefixTpe: Option[c.universe.Type], expr: c.universe.Tree, allow: Boolean): c.Expr[AccessValidator] = {
    import c.universe._

    val typesAndSymbolsBuilder = List.newBuilder[(Type, Symbol, Symbol)]

    def fullyWidened(tpe: Type) = tpe.map(_.widen)

    def isStaticOrConstructor(symbol: Symbol) =
      symbol.isStatic || (symbol.isMethod && symbol.asMethod.isConstructor)

    // TODO: check name of anonymous param?
    def isValidPrefix(body: Tree, prefix: Tree) =
      if (requiredPrefixTpe.isDefined)
        prefix.tpe <:< requiredPrefixTpe.get
      else
        isStaticOrConstructor(body.symbol)

    def accessSpec(prefix: Tree, body: Tree, implConv: Tree) =
      (fullyWidened(requiredPrefixTpe.getOrElse(prefix.tpe)), body.symbol, if (implConv != null) implConv.symbol else null)

    def extractSymbols(body: Tree) {
      body match {
        case Select(apply@Apply(fun, List(prefix)), _) if isValidPrefix(body, prefix) &&
          isStaticImplicitConversion(fun.symbol) && apply.pos == prefix.pos =>

          typesAndSymbolsBuilder += accessSpec(prefix, body, fun)

        case Select(prefix, _) if isValidPrefix(body, prefix) =>
          typesAndSymbolsBuilder += accessSpec(prefix, body, null)

        case Apply(inner, _) => extractSymbols(inner)
        case TypeApply(inner, _) => extractSymbols(inner)
        case Function(_, actualBody) => extractSymbols(actualBody)
        case Block(stats, finalExpr) =>
          (finalExpr :: stats).foreach(extractSymbols)
        case _ => c.error(body.pos, "Bad symbol specification syntax: ")
      }
    }

    extractSymbols(expr)

    val typesAndSymbols = typesAndSymbolsBuilder.result()

    def reifiedTypeExpr(tpe: Type) = {
      val s = tpe.typeSymbol
      if (s != null && s.isJava && isModuleOrPackage(s)) // reifying Java classes as singleton (module) types doesn't work...
        c.literalNull
      else
        reify(c.Expr[ru.TypeTag[_]](c.reifyType(treeBuild.mkRuntimeUniverseRef, EmptyTree, tpe)).splice.tpe)
    }

    val reifiedTypeSignaturePairs: List[Tree] = typesAndSymbols.map {
      case (tpe, ms, ic) => reify(
        MemberAccessSpec(
          reifiedTypeExpr(tpe).splice,
          c.literal(memberSignature(ms)).splice,
          c.literal(memberSignature(ic)).splice))
        .tree
    }

    val typesAndMethodsListExpr: c.Expr[List[MemberAccessSpec]] =
      c.Expr(Apply(Select(reify(List).tree, newTermName("apply")), reifiedTypeSignaturePairs))

    reify(new TypeMembersValidator(typesAndMethodsListExpr.splice, c.literal(allow).splice))
  }
}