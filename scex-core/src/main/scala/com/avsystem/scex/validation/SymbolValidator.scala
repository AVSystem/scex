package com.avsystem.scex.validation

import SymbolValidator._
import com.avsystem.scex.util.CommonUtils._
import java.{util => ju, lang => jl}
import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.reflect.api.{Universe, TypeCreator}
import scala.reflect.macros.Context
import com.avsystem.scex.util.MacroUtils

class SymbolValidator(accessSpecs: List[MemberAccessSpec]) {

  def isInvocationAllowed(c: Context)
    (objType: c.universe.Type, invocationSymbol: c.universe.Symbol, invocationConvOpt: Option[c.universe.Symbol]): Boolean = {

    import c.universe._
    val macroUtils = MacroUtils(c)
    import macroUtils._

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
    } getOrElse false
  }

  lazy val referencedJavaClasses = accessSpecs.collect({
    case MemberAccessSpec(typeInfo, _, _, true)
      if typeInfo.clazz.isDefined && typeInfo.isJava =>
      hierarchy(typeInfo.clazz.get)
  }).flatten.toSet
}

object SymbolValidator {

  case class MemberAccessSpec(typeInfo: TypeInfo, member: String, implicitConv: Option[String], allow: Boolean)

  implicit class WildcardMemberAccess(wrapped: Any) {
    private def elidedByMacro =
      throw new NotImplementedError("Cannot use WildcardMemberAccess methods outside of symbol validator DSL")

    /**
     * Allows for calling any method on given type
     */
    def anyMethod = elidedByMacro

    /**
     * Allows for calling any overloaded variant of method with given name
     */
    def anyMethodNamed(name: String) = elidedByMacro

    /**
     * Allows for creating new instances of given type using any public constructor.
     */
    def anyConstructor = elidedByMacro

    /**
     * Allows for creating new instances of given type using constructor with given signature.
     */
    def constructorWithSignature(signature: String) = elidedByMacro

    /**
     * Allows for calling any Scala val or var getters
     */
    def anyScalaGetter = elidedByMacro

    /**
     * Allows for calling any Scala var setters
     */
    def anyScalaSetter = elidedByMacro

    /**
     * Allows for calling any Java bean getters
     */
    def anyBeanGetter = elidedByMacro

    /**
     * Allows for calling any Java bean setters
     */
    def anyBeanSetter = elidedByMacro
  }

  def allow(expr: Any): List[SymbolValidator.MemberAccessSpec] = macro allow_impl

  def allow_impl(c: Context)(expr: c.Expr[Any]) =
    extractMemberAccessSpecs(c)(expr, allow = true)

  def deny(expr: Any): List[SymbolValidator.MemberAccessSpec] = macro deny_impl

  def deny_impl(c: Context)(expr: c.Expr[Any]) =
    extractMemberAccessSpecs(c)(expr, allow = false)

  private def extractMemberAccessSpecs(c: Context)(expr: c.Expr[Any], allow: Boolean): c.Expr[List[MemberAccessSpec]] = {
    import c.universe._
    val macroUtils = MacroUtils(c)
    import macroUtils._

    def accessSpec(prefix: Tree, body: Tree, implConv: Option[Tree]) = {
      (prefix.tpe, body.symbol, implConv.map(_.symbol))
    }

    def accessSpecsForMethodNamed(tpe: Type, name: Name) = {
      val member = tpe.member(name)
      if (member.isTerm) {
        Some(member.asTerm.alternatives.withFilter(_.isMethod).map(symbol => (tpe, symbol, None)))
      } else {
        None
      }
    }

    def accessSpecsFor(tpe: Type, predicate: Symbol => Boolean) = {
      tpe.members.withFilter(predicate).map(symbol => (tpe, symbol, None)).toList
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

    def extractSymbols(body: Tree): List[(Type, Symbol, Option[Symbol])] = body match {
      case WildcardMemberAccessMethod(prefix, "anyMethod") =>
        accessSpecsFor(prefix.tpe, _.isMethod)

      case Apply(WildcardMemberAccessMethod(prefix, "anyMethodNamed"), List(memberNameLiteral@LiteralString(memberName))) =>
        accessSpecsForMethodNamed(prefix.tpe, newTermName(memberName).encodedName).getOrElse {
          c.error(memberNameLiteral.pos, s"${prefix.tpe.map(_.widen)} has no members named $memberName")
          Nil
        }

      case WildcardMemberAccessMethod(prefix, "anyConstructor") =>
        accessSpecsForMethodNamed(prefix.tpe, nme.CONSTRUCTOR).getOrElse {
          c.error(body.pos, s"${prefix.tpe.map(_.widen)} has no constructors")
          Nil
        }

      case Apply(WildcardMemberAccessMethod(prefix, "constructorWithSignature"), List(signatureLiteral@LiteralString(signature))) =>
        val constructors = prefix.tpe.member(nme.CONSTRUCTOR).asTerm.alternatives
        constructors.find(ctor => ctor.typeSignature.toString == signature) match {
          case Some(symbol) => List((prefix.tpe, symbol, None))
          case None =>
            val widenedTpe = prefix.tpe.map(_.widen)
            val availableSignatures = constructors.map(_.typeSignature.toString).mkString("; ")

            c.error(signatureLiteral.pos,
              s"$widenedTpe has no constructor with signature $signature. " +
                s"Available constructors have signatures: $availableSignatures")

            Nil
        }

      case WildcardMemberAccessMethod(prefix, "anyScalaGetter") =>
        accessSpecsFor(prefix.tpe, s => s.isMethod && s.asMethod.isGetter)

      case WildcardMemberAccessMethod(prefix, "anyScalaSetter") =>
        accessSpecsFor(prefix.tpe, s => s.isMethod && s.asMethod.isSetter)

      case WildcardMemberAccessMethod(prefix, "anyBeanGetter") =>
        accessSpecsFor(prefix.tpe, isBeanGetter)

      case WildcardMemberAccessMethod(prefix, "anyBeanSetter") =>
        accessSpecsFor(prefix.tpe, isBeanSetter)

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
