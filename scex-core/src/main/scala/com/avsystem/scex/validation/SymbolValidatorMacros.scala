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

    def reifyAccessSpec(prefixTpe: Type, symbol: Symbol, implConv: Symbol) = reify {
      List(MemberAccessSpec(
        reifyTypeInfo(prefixTpe).splice,
        reifySignature(symbol).splice,
        reifySignature(implConv).splice,
        c.literal(allow).splice))
    }

    def checkPrefix(required: Option[(Symbol, Type)], prefix: Tree) = (required, prefix) match {
      case (Some((requiredSymbol, requiredTpe)), Ident(_))
        if prefix.symbol == requiredSymbol && prefix.tpe <:< requiredTpe =>
        requiredTpe
      case (None, _) if isModuleOrPackage(prefix.symbol) =>
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

    case class ParsedWildcardSelector(prefixTpe: Type, sourceTpe: Type, scope: List[MethodSymbol], implConv: Symbol) {
      def filterScope(pred: MethodSymbol => Boolean) =
        copy(scope = scope.filter(pred))

      def filterScopeNot(pred: MethodSymbol => Boolean) =
        copy(scope = scope.filterNot(pred))

      def reifyMemberAccessSpecs = reifyFlattenLists(scope.map { method =>
        reifyAccessSpec(prefixTpe, method, implConv)
      })
    }

    val InvalidParsedWildcardSelector = ParsedWildcardSelector(NoType, NoType, Nil, NoSymbol)

    def parseWildcardSelector(requiredPrefix: Option[(Symbol, Type)], tree: Tree): ParsedWildcardSelector = tree match {
      // prefix implicitly converted to DirectWildcardSelector
      case ImplicitlyConverted(prefix, _) if hasType[DirectWildcardSelector](tree) =>
        val tpe = checkPrefix(requiredPrefix, prefix)
        ParsedWildcardSelector(tpe, tpe, publicMethods(tpe), NoSymbol)

      // SymbolValidator.allStatic[T]
      case TypeApply(Select(symbolValidatorModule, TermName("allStatic")), List(tpeTree))
        if hasType[SymbolValidator.type](symbolValidatorModule) && isJavaClass(tpeTree.symbol) =>

        val tpeWithStatics = tpeTree.symbol.companionSymbol.typeSignature
        ParsedWildcardSelector(tpeWithStatics, tpeWithStatics, publicMethods(tpeWithStatics), NoSymbol)

      // <prefix>.all
      case Select(prefix, TermName("all")) if hasType[WildcardSelector](prefix) =>
        parseWildcardSelector(requiredPrefix, prefix)

      // <prefix>.implicitlyAs[T]
      case TypeApply(Select(prefix, TermName("implicitlyAs")), List(implicitTpeTree))
        if hasType[DirectWildcardSelector](prefix) =>

        val prefixTpe = parseWildcardSelector(requiredPrefix, prefix).prefixTpe
        val implicitTpe = implicitTpeTree.tpe

        val implConv = c.inferImplicitView(EmptyTree, prefixTpe, implicitTpe, true, false, tree.pos)
        if (isImplicitConversion(implConv.symbol)) {
          val newScope = publicMethods(implicitTpe).filterNot(_.isConstructor)
          ParsedWildcardSelector(prefixTpe, implicitTpe, newScope, implConv.symbol)
        } else {
          c.error(tree.pos, s"No globally available implicit conversion from ${prefixTpe.widen} to $implicitTpe found.")
          InvalidParsedWildcardSelector
        }

      // <prefix>.constructorWithSignature(<signature>)
      case Apply(Select(prefix, TermName("constructorWithSignature")), List(Literal(Constant(signature: String))))
        if hasType[DirectWildcardSelector](prefix) =>

        val prevSelector = parseWildcardSelector(requiredPrefix, prefix)
        prevSelector.scope.find(p => p.isConstructor && p.typeSignature.toString == signature) match {
          case Some(ctor) =>
            prevSelector.copy(scope = List(ctor))
          case None =>
            val availableSignatures = prevSelector.scope.collect {
              case method if method.isConstructor => method.typeSignature.toString
            }
            c.error(tree.pos, s"Type ${prevSelector.sourceTpe.widen} has no constructor with signature $signature\n" +
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
      case Select(prefix, TermName("constructors")) if hasType[DirectMethodSubsets](prefix) =>
        parseWildcardSelector(requiredPrefix, prefix).filterScope(_.isConstructor)

      // <prefix>.methods
      case Select(prefix, TermName("methods")) if hasType[MethodSubsets](prefix) =>
        parseWildcardSelector(requiredPrefix, prefix).filterScopeNot(_.isConstructor)

      // <prefix>.methodsNamed.<methodName> or <prefix>.methodsNamed(<methodName>)
      case Apply(Select(Select(prefix, TermName("methodsNamed")), TermName("selectDynamic")), List(Literal(Constant(methodName: String))))
        if hasType[MethodSubsets](prefix) =>

        val result = parseWildcardSelector(requiredPrefix, prefix).filterScope(_.name.decoded == methodName)
        if (result.scope.isEmpty) {
          c.error(tree.pos, s"No method named $methodName found in type ${result.sourceTpe.widen}")
          InvalidParsedWildcardSelector
        } else {
          result
        }

      // <prefix>.beanGetters
      case Select(prefix, TermName("beanGetters")) if hasType[MethodSubsets](prefix) =>
        parseWildcardSelector(requiredPrefix, prefix).filterScope(isBeanGetter)

      // <prefix>.beanSetters
      case Select(prefix, TermName("beanSetters")) if hasType[MethodSubsets](prefix) =>
        parseWildcardSelector(requiredPrefix, prefix).filterScope(isBeanSetter)

      // <prefix>.scalaGetters
      case Select(prefix, TermName("scalaGetters")) if hasType[ScalaMethodSubsets](prefix) =>
        parseWildcardSelector(requiredPrefix, prefix).filterScope(_.isGetter)

      // <prefix>.scalaSetters
      case Select(prefix, TermName("scalaSetters")) if hasType[ScalaMethodSubsets](prefix) =>
        parseWildcardSelector(requiredPrefix, prefix).filterScope(_.isSetter)

      case _ =>
        c.error(tree.pos, "Bad wildcard selector syntax: ")
        InvalidParsedWildcardSelector
    }

    def extractSymbols(requiredPrefix: Option[(Symbol, Type)], body: Tree): c.Expr[List[MemberAccessSpec]] = body match {
      case Block(stats, finalExpr) =>
        reifyFlattenLists((stats :+ finalExpr).map(extractSymbols(requiredPrefix, _)))

      case _ if body.tpe =:= typeOf[CompleteWildcardSelector] =>
        parseWildcardSelector(requiredPrefix, body).reifyMemberAccessSpecs

      case NewInstance(tpeTree, _) =>
        reifyAccessSpec(checkNewInstanceTpe(requiredPrefix, tpeTree), body.symbol, NoSymbol)

      case Select(ImplicitlyConverted(prefix, fun), _) =>
        reifyAccessSpec(checkPrefix(requiredPrefix, prefix), body.symbol, fun.symbol)

      case Select(prefix, _) =>
        reifyAccessSpec(checkPrefix(requiredPrefix, prefix), body.symbol, NoSymbol)

      case Apply(inner, _) =>
        extractSymbols(requiredPrefix, inner)

      case TypeApply(inner, _) =>
        extractSymbols(requiredPrefix, inner)

      case Function(List(valdef@ValDef(_, _, prefixTpeTree, _)), actualBody)
        if body.attachments.get[SymbolValidatorOnMark.type].isDefined =>

        extractSymbols(Some((valdef.symbol, prefixTpeTree.tpe)), actualBody)

      case Function(_, actualBody) =>
        extractSymbols(requiredPrefix, actualBody)

      case _ =>
        c.error(body.pos, "Bad symbol specification syntax: ")
        reify(Nil)
    }

    extractSymbols(None, expr.tree)
  }

  def methodsNamed_impl(c: Context {type PrefixType = MethodSubsets})(name: c.Expr[String]): c.Expr[CompleteWildcardSelector] = {
    import c.universe._
    reify(c.prefix.splice.methodsNamed.selectDynamic(name.splice))
  }
}
