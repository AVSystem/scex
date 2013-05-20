package com.avsystem.scex.validation

import com.avsystem.scex.util.MacroUtils
import java.{util => ju, lang => jl}
import scala.reflect.api.TypeCreator
import scala.reflect.macros.Context
import scala.reflect.runtime.{universe => ru}

object SymbolValidatorMacros {

  import SymbolValidator._

  // marks trees that were wrapped in SymbolValidator.on[T] invocation
  object SymbolValidatorOnMark

  def allow_impl(c: Context)(expr: c.Expr[Any]) =
    extractMemberAccessSpecs(c)(expr, allow = true)

  def deny_impl(c: Context)(expr: c.Expr[Any]) =
    extractMemberAccessSpecs(c)(expr, allow = false)

  def on_impl[T](c: Context)(expr: c.Expr[T => Any]): c.Expr[T => Any] = {
    expr.tree.updateAttachment(SymbolValidatorOnMark)
    expr
  }

  private def extractMemberAccessSpecs(c: Context)(expr: c.Expr[Any], allow: Boolean): c.Expr[List[MemberAccessSpec]] = {
    import c.universe._
    val macroUtils = MacroUtils(c)
    import macroUtils._

    def accessSpec(prefixTpe: Type, body: Tree, implConv: Option[Tree]) = {
      (prefixTpe, body.symbol, implConv.map(_.symbol))
    }

    def accessSpecsForMethodNamed(tpe: Type, name: Name) = {
      val member = tpe.member(name)
      val viableMethods = if (member.isTerm) {
        member.asTerm.alternatives.filter(s => s.isPublic && s.isMethod)
      } else {
        Nil
      }
      if (viableMethods.nonEmpty) {
        Some(viableMethods.map(symbol => (tpe, symbol, None)))
      } else {
        None
      }
    }

    def accessSpecsFor(tpe: Type, predicate: Symbol => Boolean) = {
      tpe.members.withFilter(s => s.isPublic && predicate(s)).map(symbol => (tpe, symbol, None)).toList
    }

    def accessSpecsForDeclarations(tpe: Type, predicate: Symbol => Boolean) = {
      tpe.declarations.withFilter(s => s.isPublic && predicate(s)).map(symbol => (tpe, symbol, None)).toList
    }

    // extractor that matches calls to WildcardMemberAccess methods
    object WildcardMemberAccessMethod {
      def unapply(tree: Tree) = tree match {
        case Select(implConv@ImplicitlyConverted(prefix, fun), termName)
          if termName.isTermName && implConv.tpe <:< typeOf[WildcardMemberAccess] =>

          Some(prefix, termName.decoded)

        case _ =>
          None
      }
    }

    def checkPrefixTpe(required: Option[Type], prefix: Tree) = required match {
      case Some(requiredTpe) if prefix.tpe <:< requiredTpe =>
        requiredTpe
      case None =>
        prefix.tpe
      case _ =>
        c.error(prefix.pos, "Bad symbol specification syntax:")
        typeOf[Nothing]
    }

    def extractSymbols(prefixTpe: Option[Type], body: Tree): List[(Type, Symbol, Option[Symbol])] = body match {
      case WildcardMemberAccessMethod(prefix, "anyMethod") =>
        accessSpecsFor(checkPrefixTpe(prefixTpe, prefix), s => s.isMethod && !s.asMethod.isConstructor)

      case WildcardMemberAccessMethod(prefix, "anyDeclaredMethod") =>
        accessSpecsForDeclarations(checkPrefixTpe(prefixTpe, prefix), s => s.isMethod && !s.asMethod.isConstructor)

      case Apply(WildcardMemberAccessMethod(prefix, "anyMethodNamed"), List(memberNameLiteral@LiteralString(methodName))) =>
        val tpe = checkPrefixTpe(prefixTpe, prefix)
        accessSpecsForMethodNamed(tpe, newTermName(methodName).encodedName).getOrElse {
          c.error(memberNameLiteral.pos, s"${tpe.map(_.widen)} has no methods named $methodName")
          Nil
        }

      case WildcardMemberAccessMethod(prefix, "anyConstructor") =>
        val tpe = checkPrefixTpe(prefixTpe, prefix)
        accessSpecsForMethodNamed(tpe, nme.CONSTRUCTOR).getOrElse {
          c.error(body.pos, s"${tpe.map(_.widen)} has no constructors")
          Nil
        }

      case Apply(WildcardMemberAccessMethod(prefix, "constructorWithSignature"), List(signatureLiteral@LiteralString(signature))) =>

        val tpe = checkPrefixTpe(prefixTpe, prefix)
        val constructors = tpe.member(nme.CONSTRUCTOR).asTerm.alternatives
        constructors.find(ctor => ctor.typeSignature.toString == signature) match {
          case Some(symbol) => List((tpe, symbol, None))
          case None =>
            val widenedTpe = tpe.map(_.widen)
            val availableSignatures = constructors.map(_.typeSignature.toString).mkString("; ")

            c.error(signatureLiteral.pos,
              s"$widenedTpe has no constructor with signature $signature. " +
                s"Available constructors have signatures: $availableSignatures")

            Nil
        }

      case WildcardMemberAccessMethod(prefix, "anyScalaGetter") =>
        accessSpecsFor(checkPrefixTpe(prefixTpe, prefix), s => s.isMethod && s.asMethod.isGetter)

      case WildcardMemberAccessMethod(prefix, "anyScalaSetter") =>
        accessSpecsFor(checkPrefixTpe(prefixTpe, prefix), s => s.isMethod && s.asMethod.isSetter)

      case WildcardMemberAccessMethod(prefix, "anyBeanGetter") =>
        accessSpecsFor(checkPrefixTpe(prefixTpe, prefix), isBeanGetter)

      case WildcardMemberAccessMethod(prefix, "anyBeanSetter") =>
        accessSpecsFor(checkPrefixTpe(prefixTpe, prefix), isBeanSetter)

      case Select(ImplicitlyConverted(prefix, fun), _) =>
        List(accessSpec(checkPrefixTpe(prefixTpe, prefix), body, Some(fun)))

      case Select(prefix, _) =>
        List(accessSpec(checkPrefixTpe(prefixTpe, prefix), body, None))

      case Apply(inner, _) =>
        extractSymbols(prefixTpe, inner)

      case TypeApply(inner, _) =>
        extractSymbols(prefixTpe, inner)

      case Function(List(ValDef(_, _, prefixTpeTree, _)), actualBody)
        if body.attachments.get[SymbolValidatorOnMark.type].isDefined =>

        extractSymbols(Some(prefixTpeTree.tpe), actualBody)

      case Function(_, actualBody) =>
        extractSymbols(prefixTpe, actualBody)

      case Block(stats, finalExpr) =>
        (finalExpr :: stats).flatMap(extractSymbols(prefixTpe, _: Tree))

      case _ =>
        c.error(body.pos, "Bad symbol specification syntax: ")
        Nil
    }

    val rawAccessSpecs = extractSymbols(None, expr.tree)

    def reifySignature(symbol: Symbol) =
      c.literal(memberSignature(symbol))

    def reifyTypeInfo(tpe: Type) = {
      val widenedTpe = tpe.map(_.widen)

      val Block(List(_, _), Apply(_, List(_, typeCreatorTree))) =
        c.reifyType(treeBuild.mkRuntimeUniverseRef, EmptyTree, widenedTpe)

      reify(new TypeInfo(
        c.Expr[TypeCreator](typeCreatorTree).splice,
        reifyRuntimeClassOpt(tpe).splice,
        c.literal(tpe.typeSymbol.isJava).splice,
        c.literal(widenedTpe.toString).splice))
    }

    val reifiedAccessSpecs: List[Tree] = rawAccessSpecs.map {
      case (tpe, ms, ic) => reify(
        MemberAccessSpec(
          reifyTypeInfo(tpe).splice,
          reifySignature(ms).splice,
          reifyOption(ic, reifySignature(_: Symbol)).splice,
          c.literal(allow).splice))
        .tree
    }

    val typesAndMethodsListExpr: c.Expr[List[MemberAccessSpec]] =
      c.Expr(Apply(Select(reify(List).tree, newTermName("apply")), reifiedAccessSpecs))

    typesAndMethodsListExpr
  }

}
