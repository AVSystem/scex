package com.avsystem.scex.symboldsl

import com.avsystem.scex.util.{MacroUtils, TypeWrapper}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.internal.Flags
import scala.reflect.macros.blackbox

/**
  * Author: ghik
  * Created: 11/14/14.
  */
abstract class SymbolInfoParser[C <: blackbox.Context](val c: C) extends MacroUtils {
  lazy val universe: c.universe.type = c.universe

  import c.universe._

  def dslObject: Tree
  def defaultPayload: c.Tree

  def getType(typeTree: Tree): Type =
    c.typecheck(typeTree, c.TYPEmode).tpe

  val dslObjectType = getType(tq"$dslObject.type")
  val plusType = getType(tq"$dslObject.plus")
  val minusType = getType(tq"$dslObject.minus")
  val PayloadType = getType(tq"$dslObject.Payload")
  val AttachedPayloadType = getType(tq"$dslObject.AttachedPayload")
  val DirectWildcardSelectorType = getType(tq"$dslObject.DirectWildcardSelector")
  val WildcardSelectorType = getType(tq"$dslObject.WildcardSelector")
  val ScopeSpecifiersType = getType(tq"$dslObject.ScopeSpecifiers")
  val DirectMemberSubsetsType = getType(tq"$dslObject.DirectMemberSubsets")
  val MemberSubsetsType = getType(tq"$dslObject.MemberSubsets")
  val ScalaMemberSubsetsType = getType(tq"$dslObject.ScalaMemberSubsets")
  val CompleteWildcardSelectorType = getType(tq"$dslObject.CompleteWildcardSelector")

  val ScalaPkg = q"_root_.scala"
  val CollectionPkg = q"$ScalaPkg.collection"
  val ListObj = q"$CollectionPkg.immutable.List"
  val NilObj = q"$CollectionPkg.immutable.Nil"
  val NoneObj = q"$ScalaPkg.None"
  val SomeObj = q"$ScalaPkg.Some"
  val TypeInfoCls = tq"$ScexPkg.symboldsl.TypeInfo"
  val SymbolInfoCls = tq"$ScexPkg.symboldsl.SymbolInfo"
  val SymbolInfoObj = q"$ScexPkg.symboldsl.SymbolInfo"

  import com.avsystem.scex.symboldsl.SymbolInfoParser._

  class TypeKey(val tpe: Type) extends TypeWrapper((c.universe, tpe): (c.universe.type, c.universe.Type))

  private val typeInfos: mutable.Map[TypeKey, TermName] = new mutable.HashMap[TypeKey, TermName]

  // transforms list of expressions of type List[SymbolInfo[T]] to single expression
  // of type List[SymbolInfo[T]] that represents flattened original list of lists
  def reifyFlattenLists(listExprs: List[Tree]) = {
    q"""
      val b = new $CollectionPkg.mutable.ListBuffer[$SymbolInfoCls[$PayloadType]]
      ..${listExprs.map(listExpr => q"b ++= $listExpr")}
      b.result()
    """
  }

  def reifyRuntimeClassOpt(tpe: Type): Tree =
    if (tpe == NoType || isJavaStaticType(tpe)) {
      q"$NoneObj"
    } else {
      q"$SomeObj(${c.reifyRuntimeClass(tpe)})"
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
          (oldSymbol, existentialSymbol(oldSymbol.fullName.replace(".", "_"), oldSymbol.typeSignature))
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
        if (ann.tree.tpe =:= plusType) Some(true)
        else if (ann.tree.tpe =:= minusType) Some(false)
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
    val Block(List(_, _), Apply(_, List(_, typeCreatorTree))) =
      c.reifyType(internal.gen.mkRuntimeUniverseRef, EmptyTree, tpe)

    q"new $TypeInfoCls($typeCreatorTree, ${reifyRuntimeClassOpt(tpe)}, ${tpe.typeSymbol.isJava}, ${tpe.toString})"
  }

  def typeInfoIdent(tpe: Type) = {
    val typeToReify = existentialize(detachExistentials(tpe.map(_.widen)))
    Ident(typeInfos.getOrElseUpdate(new TypeKey(typeToReify), TermName(c.freshName())))
  }

  val reifyImplicitConvSpec =
    (tree: Tree) => q"${path(tree)}"

  def reifySymbolInfo(prefixTpe: Type, payloadTree: Tree, symbol: Symbol, implicitConv: Option[Tree]) =
    q"""
      $ListObj($SymbolInfoObj[$PayloadType](
        ${typeInfoIdent(prefixTpe)},
        ${memberSignature(symbol)},
        ${reifyOption(implicitConv, reifyImplicitConvSpec)},
        $payloadTree
      ))
    """

  def unwrapConvertedPrefix(prefix: Tree) = prefix match {
    case TypeApply(Select(convTree@ImplicitlyConverted(actualPrefix, fun), TermName("as")), List(_))
      if hasType(convTree, DirectWildcardSelectorType) => actualPrefix
    case _ => prefix
  }

  def checkPrefix(required: Option[(Symbol, Type)], prefix: Tree) = (required, prefix) match {
    case (Some((requiredSymbol, requiredTpe)), Ident(_))
      if prefix.symbol == requiredSymbol && prefix.tpe <:< requiredTpe =>
      requiredTpe
    case (None, _) if prefix.symbol.isModule ||
      (prefix.symbol.isStatic && prefix.symbol.isMethod && prefix.symbol.asMethod.isStable) =>
      prefix.tpe
    case _ =>
      c.abort(prefix.pos, "Bad prefix: " + show(prefix))
  }

  def checkNewInstanceTpe(required: Option[(Symbol, Type)], tpeTree: Tree) = required match {
    case Some((_, requiredTpe)) if tpeTree.tpe <:< requiredTpe =>
      requiredTpe
    case None =>
      tpeTree.tpe
    case _ =>
      c.abort(tpeTree.pos, "Bad symbol specification syntax:")
  }

  case class ParsedWildcardSelector(prefixTpe: Type, payloadTree: Tree, scope: List[TermSymbol], implConv: Option[(Tree, Type)]) {
    def filterScope(pred: TermSymbol => Boolean) =
      copy(scope = scope.filter(pred))

    def filterScopeNot(pred: TermSymbol => Boolean) =
      copy(scope = scope.filterNot(pred))

    // have one reified type and implicit conversion spec for all MemberAccessSpecs generated from wildcard
    private def reifySymbolInfo(member: TermSymbol) =
      q"$ListObj($SymbolInfoObj[$PayloadType](prefixTypeInfo, ${memberSignature(member)}, implConvOpt, payload))"

    def reifySymbolInfos =
      q"""
        val prefixTypeInfo = ${typeInfoIdent(prefixTpe)}
        val implConvOpt = ${reifyOption(implConv.map(_._1), reifyImplicitConvSpec)}
        val payload = $payloadTree

        // separate method to avoid "Code size too large" error
        def fromWildcard = ${reifyFlattenLists(scope.map(m => reifySymbolInfo(m)))}
        fromWildcard
       """

    val sourceTpe = implConv.map(_._2).getOrElse(prefixTpe)
  }

  lazy val InvalidParsedWildcardSelector = ParsedWildcardSelector(NoType, EmptyTree, Nil, None)

  def parseWildcardSelector(requiredPrefix: Option[(Symbol, Type)], payloadTree: Tree, tree: Tree): ParsedWildcardSelector = tree match {
    // prefix implicitly converted to DirectWildcardSelector
    case ImplicitlyConverted(prefix, _) if hasType(tree, DirectWildcardSelectorType) =>
      val tpe = checkPrefix(requiredPrefix, unwrapConvertedPrefix(prefix))
      ParsedWildcardSelector(tpe, payloadTree, accessibleMembers(tpe), None)

    // SymbolValidator.allStatic[T]
    case TypeApply(Select(symbolDslModule, TermName("allStatic")), List(tpeTree))
      if hasType(symbolDslModule, dslObjectType) && isJavaClass(tpeTree.symbol) =>

      val tpeWithStatics = tpeTree.symbol.companion.typeSignature
      ParsedWildcardSelector(tpeWithStatics, payloadTree, accessibleMembers(tpeWithStatics), None)

    // <prefix>.all
    case Select(prefix, TermName("all")) if hasType(prefix, WildcardSelectorType) =>
      parseWildcardSelector(requiredPrefix, payloadTree, prefix)

    // <prefix>.implicitlyAs[T]
    case TypeApply(Select(prefix, TermName("implicitlyAs")), List(implicitTpeTree))
      if hasType(prefix, DirectWildcardSelectorType) =>

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
      if hasType(prefix, DirectWildcardSelectorType) =>

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
    case Select(prefix, TermName("declared")) if hasType(prefix, ScopeSpecifiersType) =>
      val prevSelector = parseWildcardSelector(requiredPrefix, payloadTree, prefix)
      val sourceTpeSymbol = prevSelector.sourceTpe.typeSymbol
      prevSelector.filterScope(_.owner == sourceTpeSymbol)

    // <prefix>.introduced
    case Select(prefix, TermName("introduced")) if hasType(prefix, ScopeSpecifiersType) =>
      val prevSelector = parseWildcardSelector(requiredPrefix, payloadTree, prefix)
      val sourceTpeSymbol = prevSelector.sourceTpe.typeSymbol
      // TODO: decide what "introduced" exactly means for scala val/var getters and setters
      prevSelector.filterScope(m => m.owner == sourceTpeSymbol && m.overrides.isEmpty)

    // <prefix>.constructors
    case Select(prefix, TermName("constructors")) if hasType(prefix, DirectMemberSubsetsType) =>
      parseWildcardSelector(requiredPrefix, payloadTree, prefix).filterScope(isConstructor)

    // <prefix>.members
    case Select(prefix, TermName("members")) if hasType(prefix, MemberSubsetsType) =>
      parseWildcardSelector(requiredPrefix, payloadTree, prefix).filterScopeNot(m => isConstructor(m) || isFromToplevelType(m))

    // <prefix>.membersNamed.<methodName>
    case Apply(Select(Select(prefix, TermName("membersNamed")), TermName("selectDynamic")), List(Literal(Constant(name: String))))
      if hasType(prefix, MemberSubsetsType) =>

      val termName = TermName(name).encodedName
      val result = parseWildcardSelector(requiredPrefix, payloadTree, prefix).filterScope(_.name == termName)
      if (result.scope.isEmpty) {
        c.error(tree.pos, s"No member named $name found in type ${result.sourceTpe.widen}")
        InvalidParsedWildcardSelector
      } else result

    // <prefix>.membersNamed(<methodName>)
    case Apply(Select(prefix, TermName("membersNamed")), nameTrees)
      if hasType(prefix, MemberSubsetsType) =>

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
    case Select(prefix, TermName("beanGetters")) if hasType(prefix, MemberSubsetsType) =>
      parseWildcardSelector(requiredPrefix, payloadTree, prefix).filterScope(isBeanGetter)

    // <prefix>.beanSetters
    case Select(prefix, TermName("beanSetters")) if hasType(prefix, MemberSubsetsType) =>
      parseWildcardSelector(requiredPrefix, payloadTree, prefix).filterScope(isBeanSetter)

    // <prefix>.scalaGetters
    case Select(prefix, TermName("scalaGetters")) if hasType(prefix, ScalaMemberSubsetsType) =>
      parseWildcardSelector(requiredPrefix, payloadTree, prefix).filterScope(_.isGetter)

    // <prefix>.scalaSetters
    case Select(prefix, TermName("scalaSetters")) if hasType(prefix, ScalaMemberSubsetsType) =>
      parseWildcardSelector(requiredPrefix, payloadTree, prefix).filterScope(_.isSetter)

    case _ =>
      c.error(tree.pos, "Bad wildcard selector syntax: " + showRaw(tree))
      InvalidParsedWildcardSelector
  }

  def extractSymbols(requiredPrefix: Option[(Symbol, Type)], payloadTree: Tree, body: Tree): Tree = body match {
    case Block(stats, finalExpr) =>
      reifyFlattenLists((stats :+ finalExpr).map(extractSymbols(requiredPrefix, payloadTree, _)))

    case Apply(Select(ImplicitlyConverted(inner, _), DecodedTermName("-->")), List(newPayloadTree))
      if hasType(body, AttachedPayloadType) =>
      extractSymbols(requiredPrefix, newPayloadTree, inner)

    case _ if body.tpe =:= CompleteWildcardSelectorType =>
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

    case Typed(inner, _) =>
      extractSymbols(requiredPrefix, payloadTree, inner)

    case Function(List(valdef@ValDef(_, _, prefixTpeTree, _)), actualBody)
      if internal.attachments(body).get[SymbolDslOnMark.type].isDefined =>

      extractSymbols(Some((valdef.symbol, prefixTpeTree.tpe)), payloadTree, actualBody)

    case Function(_, actualBody) =>
      extractSymbols(requiredPrefix, payloadTree, actualBody)

    case Literal(Constant(())) =>
      q"$NilObj"

    case _ =>
      c.abort(body.pos, "Bad symbol specification syntax: " + showRaw(body))
  }

  def extractSymbolInfos(tree: Tree): Tree = {
    val symbolInfos = extractSymbols(None, defaultPayload, tree)

    val typeInfoDefs = typeInfos.iterator.map {
      case (typeKey, termName) =>
        ValDef(Modifiers(), termName, TypeTree(), reifyTypeInfo(typeKey.tpe))
    }.toList

    Block(typeInfoDefs, symbolInfos)
  }
}

object SymbolInfoParser {
  object SymbolDslOnMark

  object AlreadyReified

  def on_impl[T](c: blackbox.Context)(expr: c.Expr[T => Any]): c.Expr[T => Any] = {
    c.internal.updateAttachment(expr.tree, SymbolDslOnMark)
    expr
  }

}
