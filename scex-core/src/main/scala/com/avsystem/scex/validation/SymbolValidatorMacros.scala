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

  private def extractMemberAccessSpecs(c: Context)(expr: c.Expr[Any], allow: Boolean): c.Expr[List[MemberAccessSpec]] = {
    import c.universe._
    val macroUtils = MacroUtils(c)
    import macroUtils.{c => _, _}

    // transforms list of expressions of type List[MemberAccessSpec] to single expression
    // of type List[MemberAccessSpec] that represents flattened original list of lists
    def reifyFlattenLists(listExprs: List[c.Expr[List[MemberAccessSpec]]]) = {
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

    val reifyImplicitConvSpec: ((Tree, Type)) => c.Expr[(String, TypeInfo)] = {
      case (implicitConvTree, implicitTpe) =>
        reify((c.literal(path(implicitConvTree)).splice, reifyTypeInfo(implicitTpe).splice))
    }

    def reifyAccessSpec(prefixTpe: Type, symbol: Symbol, implicitConv: Option[(Tree, Type)]) = reify {
      List(MemberAccessSpec(
        reifyTypeInfo(prefixTpe).splice,
        c.literal(memberSignature(symbol)).splice,
        reifyOption(implicitConv, reifyImplicitConvSpec).splice,
        c.literal(allow).splice))
    }

    def checkPrefix(required: Option[(Symbol, Type)], prefix: Tree) = (required, prefix) match {
      case (Some((requiredSymbol, requiredTpe)), Ident(_))
        if prefix.symbol == requiredSymbol && prefix.tpe <:< requiredTpe =>
        requiredTpe
      case (None, _) if prefix.symbol.isModule =>
        prefix.tpe
      case _ =>
        c.error(prefix.pos, "Bad prefix: " + show(prefix))
        typeOf[Nothing]
    }

    def checkNewInstanceTpe(required: Option[(Symbol, Type)], tpeTree: Tree) = required match {
      case Some((_, requiredTpe)) if tpeTree.tpe <:< requiredTpe =>
        requiredTpe
      case None =>
        tpeTree.tpe
      case _ =>
        c.error(tpeTree.pos, "Bad symbol specification syntax:")
        typeOf[Nothing]
    }

    case class ParsedWildcardSelector(prefixTpe: Type, scope: List[TermSymbol], implConv: Option[(Tree, Type)]) {
      def filterScope(pred: TermSymbol => Boolean) =
        copy(scope = scope.filter(pred))

      def filterScopeNot(pred: TermSymbol => Boolean) =
        copy(scope = scope.filterNot(pred))

      // have one reified type and implicit conversion spec for all MemberAccessSpecs generated from wildcard
      private def reifyAccessSpec(member: TermSymbol) = reify {
        List(MemberAccessSpec(
          c.Expr[TypeInfo](Ident(newTermName("prefixTypeInfo"))).splice,
          c.literal(memberSignature(member)).splice,
          c.Expr[Option[(String, TypeInfo)]](Ident(newTermName("implConvOpt"))).splice,
          c.literal(allow).splice))
      }

      def reifyMemberAccessSpecs = reify {
        val prefixTypeInfo = reifyTypeInfo(prefixTpe).splice
        val implConvOpt = reifyOption(implConv, reifyImplicitConvSpec).splice
        // separate method to avoid "Code size too large" error
        def fromWildcard = reifyFlattenLists(scope.map(m => reifyAccessSpec(m))).splice
        fromWildcard
      }

      val sourceTpe = implConv.map(_._2).getOrElse(prefixTpe)
    }

    val InvalidParsedWildcardSelector = ParsedWildcardSelector(NoType, Nil, None)

    def parseWildcardSelector(requiredPrefix: Option[(Symbol, Type)], tree: Tree): ParsedWildcardSelector = tree match {
      // prefix implicitly converted to DirectWildcardSelector
      case ImplicitlyConverted(prefix, _) if hasType[DirectWildcardSelector](tree) =>
        val tpe = checkPrefix(requiredPrefix, prefix)
        ParsedWildcardSelector(tpe, accessibleMembers(tpe), None)

      // SymbolValidator.allStatic[T]
      case TypeApply(Select(symbolValidatorModule, TermName("allStatic")), List(tpeTree))
        if hasType[SymbolValidator.type](symbolValidatorModule) && isJavaClass(tpeTree.symbol) =>

        val tpeWithStatics = tpeTree.symbol.companionSymbol.typeSignature
        ParsedWildcardSelector(tpeWithStatics, accessibleMembers(tpeWithStatics), None)

      // <prefix>.all
      case Select(prefix, TermName("all")) if hasType[WildcardSelector](prefix) =>
        parseWildcardSelector(requiredPrefix, prefix)

      // <prefix>.implicitlyAs[T]
      case TypeApply(Select(prefix, TermName("implicitlyAs")), List(implicitTpeTree))
        if hasType[DirectWildcardSelector](prefix) =>

        val prefixTpe = parseWildcardSelector(requiredPrefix, prefix).prefixTpe
        val implicitTpe = implicitTpeTree.tpe

        val implConv = stripTypeApply(c.inferImplicitView(EmptyTree, prefixTpe, implicitTpe,
          silent = true, withMacrosDisabled = false, tree.pos))

        //TODO: filter out members that already exist in original type
        if (isGlobalImplicitConversion(implConv)) {
          val newScope = accessibleMembers(implicitTpe).filterNot(isConstructor)
          ParsedWildcardSelector(prefixTpe, newScope, Some((implConv, implicitTpe)))
        } else {
          c.error(tree.pos, s"No globally available implicit conversion from ${prefixTpe.widen} to $implicitTpe found.")
          InvalidParsedWildcardSelector
        }

      // <prefix>.constructorWithSignature(<signature>)
      case Apply(Select(prefix, TermName("constructorWithSignature")), List(Literal(Constant(signature: String))))
        if hasType[DirectWildcardSelector](prefix) =>

        val prevSelector = parseWildcardSelector(requiredPrefix, prefix)
        prevSelector.scope.find(p => isConstructor(p) && p.typeSignature.toString == signature) match {
          case Some(ctor) =>
            prevSelector.copy(scope = List(ctor))
          case None =>
            val availableSignatures = prevSelector.scope.collect {
              case member if isConstructor(member) => member.typeSignature.toString
            }
            c.error(tree.pos, s"Type ${prevSelector.prefixTpe.widen} has no constructor with signature $signature\n" +
              s"Signatures of available constructors are: ${availableSignatures.mkString(", ")}")
            InvalidParsedWildcardSelector
        }

      // <prefix>.declared
      case Select(prefix, TermName("declared")) if hasType[ScopeSpecifiers](prefix) =>
        val prevSelector = parseWildcardSelector(requiredPrefix, prefix)
        val sourceTpeSymbol = prevSelector.sourceTpe.typeSymbol
        prevSelector.filterScope(_.owner == sourceTpeSymbol)

      // <prefix>.introduced
      case Select(prefix, TermName("introduced")) if hasType[ScopeSpecifiers](prefix) =>
        val prevSelector = parseWildcardSelector(requiredPrefix, prefix)
        val sourceTpeSymbol = prevSelector.sourceTpe.typeSymbol
        // TODO: decide what "introduced" exactly means for scala val/var getters and setters
        prevSelector.filterScope(m => m.owner == sourceTpeSymbol && m.allOverriddenSymbols.isEmpty)

      // <prefix>.constructors
      case Select(prefix, TermName("constructors")) if hasType[DirectMemberSubsets](prefix) =>
        parseWildcardSelector(requiredPrefix, prefix).filterScope(isConstructor)

      // <prefix>.members
      case Select(prefix, TermName("members")) if hasType[MemberSubsets](prefix) =>
        parseWildcardSelector(requiredPrefix, prefix).filterScopeNot(m => isConstructor(m) || isFromToplevelType(m))

      // <prefix>.membersNamed.<methodName> or <prefix>.membersNamed(<methodName>)
      case Apply(Select(prefix, TermName("membersNamed")), nameTrees)
        if hasType[MemberSubsets](prefix) =>

        val names = nameTrees.collect {
          case LiteralString(name) => name
          case invalidTree =>
            c.error(invalidTree.pos, "You must specify member name with literal string")
            "<invalid>"
        }.toSet

        val result = parseWildcardSelector(requiredPrefix, prefix).filterScope(names contains _.name.decoded)
        val absentMembers = names diff result.scope.map(_.name.decoded).toSet
        if (absentMembers.nonEmpty) {
          absentMembers.foreach {
            name => c.error(tree.pos, s"No method named $name found in type ${result.sourceTpe.widen}")
          }
          InvalidParsedWildcardSelector
        } else {
          result
        }

      // <prefix>.beanGetters
      case Select(prefix, TermName("beanGetters")) if hasType[MemberSubsets](prefix) =>
        parseWildcardSelector(requiredPrefix, prefix).filterScope(isBeanGetter)

      // <prefix>.beanSetters
      case Select(prefix, TermName("beanSetters")) if hasType[MemberSubsets](prefix) =>
        parseWildcardSelector(requiredPrefix, prefix).filterScope(isBeanSetter)

      // <prefix>.scalaGetters
      case Select(prefix, TermName("scalaGetters")) if hasType[ScalaMemberSubsets](prefix) =>
        parseWildcardSelector(requiredPrefix, prefix).filterScope(_.isGetter)

      // <prefix>.scalaSetters
      case Select(prefix, TermName("scalaSetters")) if hasType[ScalaMemberSubsets](prefix) =>
        parseWildcardSelector(requiredPrefix, prefix).filterScope(_.isSetter)

      case _ =>
        c.error(tree.pos, "Bad wildcard selector syntax: " + showRaw(tree))
        InvalidParsedWildcardSelector
    }

    def extractSymbols(requiredPrefix: Option[(Symbol, Type)], body: Tree): c.Expr[List[MemberAccessSpec]] = body match {
      case Block(stats, finalExpr) =>
        reifyFlattenLists((stats :+ finalExpr).map(extractSymbols(requiredPrefix, _)))

      case _ if body.tpe =:= typeOf[CompleteWildcardSelector] =>
        parseWildcardSelector(requiredPrefix, body).reifyMemberAccessSpecs

      case NewInstance(tpeTree, _) =>
        reifyAccessSpec(checkNewInstanceTpe(requiredPrefix, tpeTree), body.symbol, None)

      case Select(apply@ImplicitlyConverted(prefix, fun), _) =>
        reifyAccessSpec(checkPrefix(requiredPrefix, prefix), body.symbol, Some((stripTypeApply(fun), apply.tpe)))

      case Select(prefix, _) =>
        reifyAccessSpec(checkPrefix(requiredPrefix, prefix), body.symbol, None)

      case Apply(inner, _) =>
        extractSymbols(requiredPrefix, inner)

      case TypeApply(inner, _) =>
        extractSymbols(requiredPrefix, inner)

      case Function(List(valdef@ValDef(_, _, prefixTpeTree, _)), actualBody)
        if body.attachments.get[SymbolValidatorOnMark.type].isDefined =>

        extractSymbols(Some((valdef.symbol, prefixTpeTree.tpe)), actualBody)

      case Function(_, actualBody) =>
        extractSymbols(requiredPrefix, actualBody)

      case Literal(Constant(())) =>
        reify(Nil)

      case _ =>
        c.error(body.pos, "Bad symbol specification syntax: " + showRaw(body))
        reify(Nil)
    }

    extractSymbols(None, expr.tree)
  }

  /**
   * Translates '<prefix>.membersNamed.costam' into '<prefix>.membersNamed("costam")'
   */
  def methodsNamed_selectDynamic_impl(c: Context)(name: c.Expr[String]): c.Expr[CompleteWildcardSelector] = {
    import c.universe._
    val macroUtils = MacroUtils(c)
    import macroUtils.{c => _, _}

    c.prefix.tree match {
      case Select(methodSubsets, TermName("membersNamed")) if hasType[MemberSubsets](methodSubsets) =>
        c.Expr[CompleteWildcardSelector](Apply(Select(methodSubsets, newTermName("membersNamed")), List(name.tree)))
      case _ =>
        c.error(c.enclosingPosition, "Bad symbol specification syntax:")
        c.literalNull
    }
  }
}
