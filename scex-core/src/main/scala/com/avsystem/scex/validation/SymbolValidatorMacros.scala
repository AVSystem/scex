package com.avsystem.scex.validation

import com.avsystem.scex.util.MacroUtils
import java.{util => ju, lang => jl}
import scala.reflect.api.TypeCreator
import scala.reflect.macros.Context
import scala.collection.mutable.ListBuffer


object SymbolValidatorMacros {

  import SymbolValidator._

  // marks trees that were wrapped in SymbolValidator.on[T] invocation
  object SymbolValidatorOnMark

  object AlreadyReified

  def allow_impl(c: Context)(expr: c.Expr[Any]) =
    extractMemberAccessSpecs(c)(expr, allow = true)

  def deny_impl(c: Context)(expr: c.Expr[Any]) =
    extractMemberAccessSpecs(c)(expr, allow = false)

  def on_impl[T](c: Context)(expr: c.Expr[T => Any]): c.Expr[T => Any] = {
    expr.tree.updateAttachment(SymbolValidatorOnMark)
    expr
  }

  // transforms list of expressions of type List[MemberAccessSpec] to single expression
  // of type List[MemberAccessSpec] that represents flattened original list of lists
  private def reifyFlattenLists(c: Context)(listExprs: List[c.Expr[List[MemberAccessSpec]]]) = {
    import c.universe._

    val addToBuilderStatements = listExprs.map { listExpr =>
      Apply(Select(Ident(newTermName("b")), newTermName("++=").encodedName), List(listExpr.tree))
    }
    reify {
      val b = new ListBuffer[MemberAccessSpec]
      c.Expr[Unit](Block(addToBuilderStatements, c.literalUnit.tree)).splice
      b.result()
    }
  }

  private def extractMemberAccessSpecs(c: Context)(expr: c.Expr[Any], allow: Boolean): c.Expr[List[MemberAccessSpec]] = {
    import c.universe._
    val macroUtils = MacroUtils(c)
    import macroUtils._

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

    def reifyAccessSpec(prefixTpe: Type, body: Tree, implConv: Option[Tree]) = reify {
      List(MemberAccessSpec(
        reifyTypeInfo(prefixTpe).splice,
        reifySignature(body.symbol).splice,
        reifyOption(implConv.map(_.symbol), reifySignature(_: Symbol)).splice,
        c.literal(allow).splice))
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

    def extractSymbols(prefixTpe: Option[Type], body: Tree): c.Expr[List[MemberAccessSpec]] = body match {
      case _ if body.attachments.get[AlreadyReified.type].isDefined =>
        c.Expr[List[MemberAccessSpec]](body)

      case Block(stats, finalExpr) =>
        reifyFlattenLists(c)((stats :+ finalExpr).map(extractSymbols(prefixTpe, _)))

      case _ if body.tpe <:< typeOf[CompleteWildcardSelector] =>
        parseWildcardSelector(c)(body)

      case Select(ImplicitlyConverted(prefix, fun), _) =>
        reifyAccessSpec(checkPrefixTpe(prefixTpe, prefix), body, Some(fun))

      case Select(prefix, _) =>
        reifyAccessSpec(checkPrefixTpe(prefixTpe, prefix), body, None)

      case Apply(inner, _) =>
        extractSymbols(prefixTpe, inner)

      case TypeApply(inner, _) =>
        extractSymbols(prefixTpe, inner)

      case Function(List(ValDef(_, _, prefixTpeTree, _)), actualBody)
        if body.attachments.get[SymbolValidatorOnMark.type].isDefined =>

        extractSymbols(Some(prefixTpeTree.tpe), actualBody)

      case Function(_, actualBody) =>
        extractSymbols(prefixTpe, actualBody)

      case _ =>
        c.error(body.pos, "Bad symbol specification syntax: ")
        reify(Nil)
    }

    val result = extractSymbols(None, expr.tree)
    result.tree.updateAttachment(AlreadyReified)
    result
  }

  private def parseWildcardSelector(c: Context)(tree: c.universe.Tree) = {
    import c.universe._
    val macroUtils = MacroUtils(c)
    import macroUtils._

    def prefixes(tree: Tree): List[Tree] = tree match {
      case ImplicitlyConverted(ident@Ident(_), _) if tree.tpe <:< typeOf[DirectWildcardSelector] => ident :: Nil
      case Apply(TypeApply(Select(prefix, _), _), _) => tree :: prefixes(prefix)
      case Apply(Select(prefix, _), _) => tree :: prefixes(prefix)
      case TypeApply(Select(prefix, _), _) => tree :: prefixes(prefix)
      case Select(prefix, _) => tree :: prefixes(prefix)
      case _ => c.error(tree.pos, "Bad wildcard member selector syntax: "); Nil
    }

    val prefixTrees = prefixes(tree)
    val ident = prefixTrees.last
    val identTypeSymbol = ident.tpe.map(_.widen).typeSymbol

    def publicMethods(tpe: Type) =
      tpe.members.collect { case s if s.isPublic && s.isMethod => s.asMethod}

    /* extractors for wildcard selector methods */

    // <p>.implicitlyAs[T]
    object ImplicitlyAs {
      def unapply(tree: Tree) = tree match {
        case TypeApply(Select(prefix, name), List(implicitlyAsTpe))
          if prefix.tpe <:< typeOf[DirectWildcardSelector] && name == newTermName("implicitlyAs") =>
          Some(implicitlyAsTpe.tpe)
        case _ =>
          None
      }
    }

    // <p>.all
    object All {
      def unapply(tree: Tree) = tree match {
        case Select(prefix, name)
          if prefix.tpe <:< typeOf[WildcardSelector] && name == newTermName("all") =>
          true
        case _ =>
          false
      }
    }

    def parseSelectorLtr(prefixes: List[Tree]): (Iterable[MethodSymbol], Option[Symbol]) = {
      val head :: tail = prefixes
      val (scope, implConv) = tail match {
        case Nil => (Nil, None)
        case _ => parseSelectorLtr(tail)
      }

      head match {
        case ImplicitlyAs(implicitTpe) =>
          c.inferImplicitView(ident, ident.tpe, implicitTpe, silent = true, withMacrosDisabled = false, tree.pos) match {
            case ImplicitlyConverted(_, fun) =>
              (scope, Some(fun.symbol))
            case _ =>
              c.error(head.pos, s"No static implicit conversion to type $implicitTpe found: ")
              (scope, implConv)
          }

        case Ident(_) =>
          (publicMethods(head.tpe), implConv)

        case All() =>
          (scope, implConv)

        //TODO TODO TODO

      }
    }

    reify(Nil)
  }

  def methodsNamed_impl(c: Context)(name: c.Expr[String]): c.Expr[CompleteWildcardSelector] = {
    import c.universe._
    reify((c.prefix.asInstanceOf[c.Expr[MethodSubsets]]).splice.methodsNamed.selectDynamic(name.splice))
  }
}
