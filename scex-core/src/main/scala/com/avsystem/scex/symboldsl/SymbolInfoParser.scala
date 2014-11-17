package com.avsystem.scex
package symboldsl

import com.avsystem.scex.util.MacroUtils

import scala.collection.mutable.ListBuffer
import scala.reflect.api.TypeCreator
import scala.reflect.internal.Flags
import scala.reflect.macros.whitebox

/**
 * Author: ghik
 * Created: 11/14/14.
 */
trait SymbolInfoParser[D <: SymbolDsl with Singleton] {
  val c: whitebox.Context

  import c.universe._

  def defaultPayload: c.Expr[D#Payload]

  implicit val dslTypeTag: c.TypeTag[D]
  implicit lazy val payloadTypeTag: c.TypeTag[D#Payload] = {
    c.TypeTag[D#Payload](typeOf[D].member(TypeName("Payload")).asType.toType.dealias)
  }

  import com.avsystem.scex.symboldsl.SymbolInfoParser._

  lazy val macroUtils = MacroUtils(c.universe)

  import macroUtils._

  // transforms list of expressions of type List[SymbolInfo[T]] to single expression
  // of type List[SymbolInfo[T]] that represents flattened original list of lists
  def reifyFlattenLists(listExprs: List[c.Expr[List[SymbolInfo[D#Payload]]]]) = {
    val addToBuilderStatements = listExprs.map { listExpr =>
      Apply(Select(Ident(TermName("b")), TermName("++=").encodedName), List(listExpr.tree))
    }
    reify {
      val b = new ListBuffer[SymbolInfo[D#Payload]]
      c.Expr[Unit](Block(addToBuilderStatements, c.literalUnit.tree)).splice
      b.result()
    }
  }

  def reifyRuntimeClassOpt(tpe: Type): Expr[Option[Class[_]]] =
    if (tpe == NoType || isJavaStaticType(tpe)) {
      reify(None)
    } else {
      reify(Some(c.Expr[Class[_]](c.reifyRuntimeClass(tpe)).splice))
    }

  def existentialSymbol(name: String, typeSignature: Type) = {
    val typeName = TypeName(name)
    val flags = internal.reificationSupport.FlagsRepr(Flags.DEFERRED | Flags.EXISTENTIAL)
    val result = internal.reificationSupport.newNestedSymbol(rootMirror.RootClass, typeName, NoPosition, flags, isClass = false)
    internal.reificationSupport.setInfo(result, typeSignature)
    result
  }

  /**
   * Translates this type so that all existential types in this type do not refer to the defining class,
   * as they do by default. Heavy wizardry.
   *
   * The problem is that when you reify an existential type (with `c.reifyType`), for example `Set[_]`, the
   * wildcard is reified as a Symbol whose owner is the class that used that existential type.
   * This effectively means that the definition of existential type refers the class that used it.
   * This means that when the type is finally evaluated in some universe, things will blow up if that class is
   * not visible to that universe.
   *
   * For example, this is exactly what happens if you use some existential type in the SymbolValidator DSL and
   * the symbol validator is compiled at runtime using `compileSymbolValidator` method of ScexCompiler. That dynamically
   * compiled class is not visible to the Scala compiler through classpath and when it tries to evaluate the reified
   * type, we have a nice scala.reflect.internal.MissingRequirementError in our face.
   */
  def detachExistentials(tpe: Type) = tpe.map {
    case ExistentialType(quantified, underlying) =>
      val rootSymbol = rootMirror.RootClass

      val symbolMapping = quantified.collect {
        case oldSymbol if oldSymbol.owner != rootSymbol =>
          (oldSymbol, existentialSymbol(oldSymbol.fullName.replaceAllLiterally(".", "_"), oldSymbol.typeSignature))
      }.toMap.withDefault(identity)

      val newQuantified = quantified.map(symbolMapping)
      val newUnderlying = underlying.map {
        case TypeRef(pre, sym, args) => internal.reificationSupport.TypeRef(pre, symbolMapping(sym), args)
        case t => t
      }

      internal.reificationSupport.ExistentialType(newQuantified, newUnderlying)

    case subTpe => subTpe
  }

  /**
   * Handles @plus and @minus type annotations.
   * For example `java.util.List[Number@plus]` is translated to `java.util.List[_ <: Number]`
   */
  def existentialize(tpe: Type) = {
    object PlusOrMinus {
      def unapply(ann: Annotation) =
        if (ann.tree.tpe =:= weakTypeOf[D#plus]) Some(true)
        else if (ann.tree.tpe =:= weakTypeOf[D#minus]) Some(false)
        else None
    }

    val (quantifiedBuffer, tpeToTranslate) = tpe match {
      case ExistentialType(quantified, underlying) => (ListBuffer(quantified: _*), underlying)
      case _ => (new ListBuffer[Symbol], tpe)
    }

    val translated = tpeToTranslate.map {
      case AnnotatedType(List(PlusOrMinus(plus)), bound) =>
        val bounds =
          if (plus) internal.reificationSupport.TypeBounds(typeOf[Nothing], bound)
          else internal.reificationSupport.TypeBounds(bound, typeOf[Any])

        val sym = existentialSymbol(c.freshName(), bounds)
        quantifiedBuffer += sym
        internal.reificationSupport.TypeRef(NoPrefix, sym, Nil)

      case t => t
    }

    val newQuantified = quantifiedBuffer.result()
    if (newQuantified.nonEmpty)
      internal.reificationSupport.ExistentialType(newQuantified, translated)
    else
      translated
  }

  def reifyTypeInfo(tpe: Type) = {
    val typeToReify = existentialize(detachExistentials(tpe.map(_.widen)))

    val Block(List(_, _), Apply(_, List(_, typeCreatorTree))) =
      c.reifyType(internal.gen.mkRuntimeUniverseRef, EmptyTree, typeToReify)

    reify(new TypeInfo(
      c.Expr[TypeCreator](typeCreatorTree).splice,
      reifyRuntimeClassOpt(tpe).splice,
      c.literal(tpe.typeSymbol.isJava).splice,
      c.literal(typeToReify.toString).splice))
  }

  val reifyImplicitConvSpec =
    (tree: Tree) => c.literal(path(tree))

  def reifySymbolInfo(prefixTpe: Type, payloadTree: Tree, symbol: Symbol, implicitConv: Option[Tree]) = reify {
    List(SymbolInfo[D#Payload](
      reifyTypeInfo(prefixTpe).splice,
      c.literal(memberSignature(symbol)).splice,
      reifyOption(implicitConv, reifyImplicitConvSpec).splice,
      c.Expr[D#Payload](payloadTree).splice))
  }

  def unwrapConvertedPrefix(prefix: Tree) = prefix match {
    case TypeApply(Select(convTree@ImplicitlyConverted(actualPrefix, fun), TermName("as")), List(_))
      if hasType[D#DirectWildcardSelector](convTree) => actualPrefix
    case _ => prefix
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

  case class ParsedWildcardSelector(prefixTpe: Type, payloadTree: Tree, scope: List[TermSymbol], implConv: Option[(Tree, Type)]) {
    def filterScope(pred: TermSymbol => Boolean) =
      copy(scope = scope.filter(pred))

    def filterScopeNot(pred: TermSymbol => Boolean) =
      copy(scope = scope.filterNot(pred))

    // have one reified type and implicit conversion spec for all MemberAccessSpecs generated from wildcard
    private def reifySymbolInfo(member: TermSymbol) = reify {
      List(SymbolInfo[D#Payload](
        c.Expr[TypeInfo](Ident(TermName("prefixTypeInfo"))).splice,
        c.literal(memberSignature(member)).splice,
        c.Expr[Option[String]](Ident(TermName("implConvOpt"))).splice,
        c.Expr[D#Payload](Ident(TermName("payload"))).splice))
    }

    def reifySymbolInfos = reify {
      val prefixTypeInfo = reifyTypeInfo(prefixTpe).splice
      val implConvOpt = reifyOption(implConv.map(_._1), reifyImplicitConvSpec).splice
      val payload = c.Expr[D#Payload](payloadTree).splice
      // separate method to avoid "Code size too large" error
      def fromWildcard = reifyFlattenLists(scope.map(m => reifySymbolInfo(m))).splice
      fromWildcard
    }

    val sourceTpe = implConv.map(_._2).getOrElse(prefixTpe)
  }

  lazy val InvalidParsedWildcardSelector = ParsedWildcardSelector(NoType, EmptyTree, Nil, None)

  def parseWildcardSelector(requiredPrefix: Option[(Symbol, Type)], payloadTree: Tree, tree: Tree): ParsedWildcardSelector = tree match {
    // prefix implicitly converted to DirectWildcardSelector
    case ImplicitlyConverted(prefix, _) if hasType[D#DirectWildcardSelector](tree) =>
      val tpe = checkPrefix(requiredPrefix, unwrapConvertedPrefix(prefix))
      ParsedWildcardSelector(tpe, payloadTree, accessibleMembers(tpe), None)

    // SymbolValidator.allStatic[T]
    case TypeApply(Select(symbolDslModule, TermName("allStatic")), List(tpeTree))
      if hasType[D](symbolDslModule) && isJavaClass(tpeTree.symbol) =>

      val tpeWithStatics = tpeTree.symbol.companion.typeSignature
      ParsedWildcardSelector(tpeWithStatics, payloadTree, accessibleMembers(tpeWithStatics), None)

    // <prefix>.all
    case Select(prefix, TermName("all")) if hasType[D#WildcardSelector](prefix) =>
      parseWildcardSelector(requiredPrefix, payloadTree, prefix)

    // <prefix>.implicitlyAs[T]
    case TypeApply(Select(prefix, TermName("implicitlyAs")), List(implicitTpeTree))
      if hasType[D#DirectWildcardSelector](prefix) =>

      val prefixTpe = parseWildcardSelector(requiredPrefix, payloadTree, prefix).prefixTpe
      val implicitTpe = implicitTpeTree.tpe

      val implConv = stripTypeApply(c.inferImplicitView(EmptyTree, prefixTpe, implicitTpe,
        silent = true, withMacrosDisabled = false, tree.pos))

      //TODO: filter out members that already exist in original type
      if (isGlobalImplicitConversion(implConv)) {
        val newScope = accessibleMembers(implicitTpe).filterNot(isConstructor)
        ParsedWildcardSelector(prefixTpe, payloadTree, newScope, Some((implConv, implicitTpe)))
      } else {
        c.error(tree.pos, s"No globally available implicit conversion from ${prefixTpe.widen} to $implicitTpe found.")
        InvalidParsedWildcardSelector
      }

    // <prefix>.constructorWithSignature(<signature>)
    case Apply(Select(prefix, TermName("constructorWithSignature")), List(Literal(Constant(signature: String))))
      if hasType[D#DirectWildcardSelector](prefix) =>

      val prevSelector = parseWildcardSelector(requiredPrefix, payloadTree, prefix)
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
    case Select(prefix, TermName("declared")) if hasType[D#ScopeSpecifiers](prefix) =>
      val prevSelector = parseWildcardSelector(requiredPrefix, payloadTree, prefix)
      val sourceTpeSymbol = prevSelector.sourceTpe.typeSymbol
      prevSelector.filterScope(_.owner == sourceTpeSymbol)

    // <prefix>.introduced
    case Select(prefix, TermName("introduced")) if hasType[D#ScopeSpecifiers](prefix) =>
      val prevSelector = parseWildcardSelector(requiredPrefix, payloadTree, prefix)
      val sourceTpeSymbol = prevSelector.sourceTpe.typeSymbol
      // TODO: decide what "introduced" exactly means for scala val/var getters and setters
      prevSelector.filterScope(m => m.owner == sourceTpeSymbol && m.overrides.isEmpty)

    // <prefix>.constructors
    case Select(prefix, TermName("constructors")) if hasType[D#DirectMemberSubsets](prefix) =>
      parseWildcardSelector(requiredPrefix, payloadTree, prefix).filterScope(isConstructor)

    // <prefix>.members
    case Select(prefix, TermName("members")) if hasType[D#MemberSubsets](prefix) =>
      parseWildcardSelector(requiredPrefix, payloadTree, prefix).filterScopeNot(m => isConstructor(m) || isFromToplevelType(m))

    // <prefix>.membersNamed.<methodName>
    case Apply(Select(Select(prefix, TermName("membersNamed")), TermName("selectDynamic")), List(Literal(Constant(name: String))))
      if hasType[D#MemberSubsets](prefix) =>

      val termName = TermName(name).encodedName
      val result = parseWildcardSelector(requiredPrefix, payloadTree, prefix).filterScope(_.name == termName)
      if (result.scope.isEmpty) {
        c.error(tree.pos, s"No member named $name found in type ${result.sourceTpe.widen}")
        InvalidParsedWildcardSelector
      } else result

    // <prefix>.membersNamed(<methodName>)
    case Apply(Select(prefix, TermName("membersNamed")), nameTrees)
      if hasType[D#MemberSubsets](prefix) =>

      val names = nameTrees.collect {
        case LiteralString(name) => name
        case invalidTree =>
          c.error(invalidTree.pos, "You must specify member name with literal string")
          "<invalid>"
      }.toSet

      val result = parseWildcardSelector(requiredPrefix, payloadTree, prefix).filterScope(names contains _.name.decodedName.toString)
      val absentMembers = names diff result.scope.map(_.name.decodedName.toString).toSet
      if (absentMembers.nonEmpty) {
        absentMembers.foreach {
          name => c.error(tree.pos, s"No member named $name found in type ${result.sourceTpe.widen}")
        }
        InvalidParsedWildcardSelector
      } else result

    // <prefix>.beanGetters
    case Select(prefix, TermName("beanGetters")) if hasType[D#MemberSubsets](prefix) =>
      parseWildcardSelector(requiredPrefix, payloadTree, prefix).filterScope(isBeanGetter)

    // <prefix>.beanSetters
    case Select(prefix, TermName("beanSetters")) if hasType[D#MemberSubsets](prefix) =>
      parseWildcardSelector(requiredPrefix, payloadTree, prefix).filterScope(isBeanSetter)

    // <prefix>.scalaGetters
    case Select(prefix, TermName("scalaGetters")) if hasType[D#ScalaMemberSubsets](prefix) =>
      parseWildcardSelector(requiredPrefix, payloadTree, prefix).filterScope(_.isGetter)

    // <prefix>.scalaSetters
    case Select(prefix, TermName("scalaSetters")) if hasType[D#ScalaMemberSubsets](prefix) =>
      parseWildcardSelector(requiredPrefix, payloadTree, prefix).filterScope(_.isSetter)

    case _ =>
      c.error(tree.pos, "Bad wildcard selector syntax: " + showRaw(tree))
      InvalidParsedWildcardSelector
  }

  def extractSymbols(requiredPrefix: Option[(Symbol, Type)], payloadTree: Tree, body: Tree): c.Expr[List[SymbolInfo[D#Payload]]] = body match {
    case Block(stats, finalExpr) =>
      reifyFlattenLists((stats :+ finalExpr).map(extractSymbols(requiredPrefix, payloadTree, _)))

    case Apply(Select(ImplicitlyConverted(inner, _), DecodedTermName("-->")), List(newPayloadTree)) if hasType[D#AttachedPayload](body) =>
      extractSymbols(requiredPrefix, newPayloadTree, inner)

    case _ if body.tpe =:= weakTypeOf[D#CompleteWildcardSelector] =>
      parseWildcardSelector(requiredPrefix, payloadTree, body).reifySymbolInfos

    case NewInstance(tpeTree, _) =>
      reifySymbolInfo(checkNewInstanceTpe(requiredPrefix, tpeTree), payloadTree, body.symbol, None)

    case Select(apply@ImplicitlyConverted(prefix, fun), _) =>
      reifySymbolInfo(checkPrefix(requiredPrefix, unwrapConvertedPrefix(prefix)), payloadTree, body.symbol, Some(stripTypeApply(fun)))

    case Select(prefix, _) =>
      reifySymbolInfo(checkPrefix(requiredPrefix, unwrapConvertedPrefix(prefix)), payloadTree, body.symbol, None)

    case Apply(inner, _) =>
      extractSymbols(requiredPrefix, payloadTree, inner)

    case TypeApply(inner, _) =>
      extractSymbols(requiredPrefix, payloadTree, inner)

    case Function(List(valdef@ValDef(_, _, prefixTpeTree, _)), actualBody)
      if internal.attachments(body).get[SymbolDslOnMark.type].isDefined =>

      extractSymbols(Some((valdef.symbol, prefixTpeTree.tpe)), payloadTree, actualBody)

    case Function(_, actualBody) =>
      extractSymbols(requiredPrefix, payloadTree, actualBody)

    case Literal(Constant(())) =>
      reify(Nil)

    case _ =>
      c.error(body.pos, "Bad symbol specification syntax: " + showRaw(body))
      reify(Nil)
  }

  def extractSymbolInfos(tree: Tree): c.Expr[List[SymbolInfo[D#Payload]]] =
    extractSymbols(None, defaultPayload.tree, tree)
}

object SymbolInfoParser {

  def apply[D <: SymbolDsl with Singleton](dsl: D, ctx: whitebox.Context)(defPayload: ctx.Expr[D#Payload])(implicit tag: ctx.TypeTag[D]) =
    new SymbolInfoParser[D] {
      val c: ctx.type = ctx

      val dslTypeTag = tag

      def defaultPayload = defPayload
    }

  object SymbolDslOnMark

  object AlreadyReified

  def on_impl[T](c: whitebox.Context)(expr: c.Expr[T => Any]): c.Expr[T => Any] = {
    c.internal.updateAttachment(expr.tree, SymbolDslOnMark)
    expr
  }

}
