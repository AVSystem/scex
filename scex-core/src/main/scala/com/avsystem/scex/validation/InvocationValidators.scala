package com.avsystem.scex.validation

import scala.language.experimental.macros
import java.io.{PrintWriter, FileWriter}
import scala.reflect.macros.Context
import scala.reflect.runtime.{universe => ru}

object InvocationValidators {

  /* Allow or deny everything */

  def allowAny: InvocationValidator =
    new AlwaysMatchingValidator(allow = true)

  def denyAny: InvocationValidator =
    new AlwaysMatchingValidator(allow = false)

  /* Allow or deny Scala package and module (object) members and Java static members */

  def allow(expr: Any): InvocationValidator = macro allow_impl

  def allow_impl(c: Context)(expr: c.Expr[Any]) =
    reifyTypeWithMethodsValidator(c)(None, expr.tree, allow = true)

  def deny(expr: Any): InvocationValidator = macro deny_impl

  def deny_impl(c: Context)(expr: c.Expr[Any]) =
    reifyTypeWithMethodsValidator(c)(None, expr.tree, allow = false)

  /* Allow or deny members on instances of some concrete type */

  def allowOn[T](expr: T => Any): InvocationValidator = macro allowOn_impl[T]

  def allowOn_impl[T: c.WeakTypeTag](c: Context)(expr: c.Expr[T => Any]): c.Expr[InvocationValidator] =
    allowOrDenyOnImpl(c)(expr, allow = true)

  def denyOn[T](expr: T => Any): InvocationValidator = macro denyOn_impl[T]

  def denyOn_impl[T: c.WeakTypeTag](c: Context)(expr: c.Expr[T => Any]): c.Expr[InvocationValidator] =
    allowOrDenyOnImpl(c)(expr, allow = false)

  private def allowOrDenyOnImpl[T: c.WeakTypeTag](c: Context)(expr: c.Expr[T => Any], allow: Boolean): c.Expr[InvocationValidator] = {
    import c.universe._
    expr.tree match {
      case Function(List(ValDef(_, _, TypeTree(), EmptyTree)), body) =>
        reifyTypeWithMethodsValidator(c)(Some(weakTypeOf[T]), body, allow)
      case _ =>
        c.error(expr.tree.pos, "Bad symbol specification syntax - expected function.")
        null
    }
  }

  /*
  The big macro that extracts classes and members from expressions passed to allow/deny methods
   */
  private def reifyTypeWithMethodsValidator(c: Context)(requiredPrefixTpe: Option[c.universe.Type], expr: c.universe.Tree, allow: Boolean): c.Expr[InvocationValidator] = {
    import c.universe._

    val typesAndSymbolsBuilder = List.newBuilder[(Type, Symbol)]

    def isModuleOrPackage(symbol: Symbol) = symbol != null &&
      (symbol.isModule || symbol.isModuleClass || symbol.isPackage || symbol.isPackageClass)

    def fullyWidened(tpe: Type) = tpe.map(_.widen)

    def extractSymbols(body: Tree) {
      body match {
        case Select(prefix, _)
          // TODO: check name of anonymous param?
          if (requiredPrefixTpe.isDefined && prefix.tpe <:< requiredPrefixTpe.get) ||
            (!requiredPrefixTpe.isDefined || isModuleOrPackage(body.symbol.owner)) =>

          val finalTpe = fullyWidened(requiredPrefixTpe.getOrElse(prefix.tpe))
          typesAndSymbolsBuilder += ((finalTpe, body.symbol))

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

    def runtimeClassExpr(tpe: Type) = {
      val s = tpe.typeSymbol
      if (s.isJava && isModuleOrPackage(s))
        c.literalNull
      else
        c.Expr[Class[_]](c.reifyRuntimeClass(tpe))
    }

    val typesAndMethodsListExpr: c.Expr[List[(Class[_], String)]] =
      c.Expr(Apply(Select(reify(List).tree, newTermName("apply")), typesAndSymbols.map {
        case (tpe, ms) => reify((runtimeClassExpr(tpe).splice, c.literal(s"${ms.fullName}:${ms.typeSignature}").splice)).tree
      }))

    reify(new ClassMembersValidator(typesAndMethodsListExpr.splice, c.literal(allow).splice))
  }
}