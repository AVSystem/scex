package com.avsystem.scex.util

import scala.annotation.nowarn
import scala.reflect.api.Universe

trait MacroUtils extends CrossMacroUtils {
  val universe: Universe

  import universe._

  def scexClassType(suffix: String): Type =
    rootMirror.staticClass("com.avsystem.scex." + suffix).toType

  lazy val ScexPkg = q"_root_.com.avsystem.scex"

  lazy val adapterType = scexClassType("compiler.Markers.JavaGetterAdapter")
  lazy val syntheticType = scexClassType("compiler.Markers.Synthetic")
  lazy val expressionUtilType = scexClassType("compiler.Markers.ExpressionUtil")
  lazy val profileObjectType = scexClassType("compiler.Markers.ProfileObject")

  lazy val inputAnnotType = scexClassType("compiler.annotation.Input")
  lazy val rootValueAnnotType = scexClassType("compiler.annotation.RootValue")
  lazy val rootAdapterAnnotType = scexClassType("compiler.annotation.RootAdapter")
  lazy val notValidatedAnnotType = scexClassType("compiler.annotation.NotValidated")
  lazy val templateInterpolationsType = scexClassType("compiler.TemplateInterpolations")
  lazy val splicerType = scexClassType("compiler.TemplateInterpolations.Splicer")

  lazy val any2stringadd = symAlternatives(typeOf[Predef.type].member(TermName("any2stringadd")))
    .find(_.isMethod).getOrElse(NoSymbol)
  @nowarn("msg=deprecated")
  lazy val stringAddPlus = typeOf[any2stringadd[_]].member(TermName("+").encodedName)
  lazy val stringConcat = typeOf[String].member(TermName("+").encodedName)
  lazy val safeToString = templateInterpolationsType.companion.decl(TermName("safeToString"))
  lazy val splicerToString = splicerType.decl(TermName("toString"))
  lazy val stringTpe = typeOf[String]
  lazy val booleanTpe = typeOf[Boolean]
  lazy val jBooleanTpe = typeOf[java.lang.Boolean]
  lazy val dynamicTpe = typeOf[Dynamic]
  lazy val dynamicVarAccessorTpe = scexClassType("util.DynamicVariableAccessor")

  lazy val BeanGetterNamePattern = "get(([A-Z][a-z0-9_]*)+)".r
  lazy val BooleanBeanGetterNamePattern = "is(([A-Z][a-z0-9_]*)+)".r
  lazy val BeanSetterNamePattern = "set(([A-Z][a-z0-9_]*)+)".r
  lazy val AdapterWrappedName = TermName("_wrapped")

  lazy val toplevelSymbols = Set(typeOf[Any], typeOf[AnyRef], typeOf[AnyVal]).map(_.typeSymbol)
  lazy val standardStringInterpolations = Set("s", "raw").map(name => typeOf[StringContext].member(TermName(name)))
  lazy val getClassSymbol = typeOf[Any].member(TermName("getClass"))

  object DecodedTermName {
    def unapply(name: TermName) =
      Some(name.decodedName.toString)
  }

  object DecodedTypeName {
    def unapply(name: TermName) =
      Some(name.decodedName.toString)
  }

  object LiteralString {
    def unapply(tree: Tree) = tree match {
      case Literal(Constant(str: String)) =>
        Some(str)
      case _ =>
        None
    }
  }

  // extractor that matches compiler-generated applications of static implicit conversions
  object ImplicitlyConverted {
    def unapply(tree: Tree): Option[(Tree, Tree)] = tree match {
      case Apply(fun, List(prefix))
        if isGlobalImplicitConversion(fun) && (tree.pos == NoPosition || prefix.pos == NoPosition || tree.pos == prefix.pos) =>
        Some((prefix, fun))
      case _ =>
        None
    }
  }

  object NewInstance {
    def unapply(tree: Tree) = tree match {
      case Apply(Select(New(tpeTree), termNames.CONSTRUCTOR), args) =>
        Some((tpeTree, args))
      case _ =>
        None
    }
  }

  object Applies {
    def apply(qual: Tree, valueArgLists: List[List[Tree]]): Tree =
      valueArgLists.foldLeft(qual)(Apply(_, _))

    def unapply(tree: Tree): Option[(Tree, List[List[Tree]])] = tree match {
      case Apply(Applies(qual, argLists), args) => Some((qual, argLists :+ args))
      case _ => Some((tree, Nil))
    }
  }

  object MemberCall {
    def apply(qual: Tree, name: Name, typeArgs: List[Tree], valueArgLists: List[List[Tree]]): Tree = {
      val selected = Select(qual, name)
      val typeApplied = if (typeArgs.nonEmpty) TypeApply(selected, typeArgs) else selected
      valueArgLists.foldLeft(typeApplied)(Apply(_, _))
    }

    def unapply(tree: Tree): Option[(Tree, Name, List[Tree], List[List[Tree]])] = {
      val Applies(qual, argLists) = tree
      val (typeQual, typeArgs) = qual match {
        case TypeApply(tq, ta) => (tq, ta)
        case _ => (qual, Nil)
      }
      typeQual match {
        case Select(squal, name) => Some((squal, name, typeArgs, argLists))
        case _ => None
      }
    }
  }

  object SelectDynamic {
    def unapply(tree: Tree) = tree match {
      case Apply(Select(qual, TermName("selectDynamic")), List(lit@Literal(Constant(name: String))))
        if qual.tpe != null && qual.tpe <:< dynamicTpe && lit.pos.isTransparent =>
        Some((qual, name))
      case _ => None
    }
  }

  object StringInterpolation {
    def unapply(tree: Apply) = tree match {
      case Apply(Select(StringContextApply(parts), _), args) => Some((parts, args))
      case _ => None
    }
  }

  object StringContextTree {
    def unapply(tree: Tree) = tree match {
      case Ident(name) if name.decodedName.toString == "StringContext" => true
      case Select(_, name) if name.decodedName.toString == "StringContext" => true
      case _ => false
    }
  }

  object StringContextApply {
    def unapply(tree: Tree) = tree match {
      case Apply(Select(StringContextTree(), name), parts) if name.decodedName.toString == "apply" => Some(parts)
      case Apply(StringContextTree(), parts) => Some(parts)
      case _ => None
    }
  }

  def isProperPosition(pos: Position) =
    pos != null && pos != NoPosition

  def isModuleOrPackage(symbol: Symbol) = symbol != null &&
    (symbol.isModule || symbol.isModuleClass || symbol.isPackage || symbol.isPackageClass)

  def isJavaField(symbol: Symbol) =
    symbol != null && symbol.isJava && symbol.isTerm && !symbol.isMethod && !isModuleOrPackage(symbol)

  def isConstructor(s: Symbol) =
    s.isMethod && s.asMethod.isConstructor

  def memberSignature(s: Symbol) =
    if (s != null) s"${s.fullName}${paramsSignature(s)}" else null

  def paramsSignature(s: Symbol) =
    s.info.paramLists.map(_.map(_.typeSignature.toString).mkString("(", ",", ")")).mkString

  def erasureFullName(tpe: Type) =
    tpe.erasure.typeSymbol.fullName

  def isStableTerm(s: Symbol) =
    s.isTerm && s.asTerm.isStable

  def stripTypeApply(tree: Tree): Tree = tree match {
    case TypeApply(prefix, _) => stripTypeApply(prefix)
    case _ => tree
  }

  def paramsOf(tpe: Type): (List[List[Symbol]], List[Symbol]) = tpe match {
    case PolyType(tp, resultType) =>
      paramsOf(resultType)
    case MethodType(params, resultType) =>
      val (moreParams, implParams) = paramsOf(resultType)
      if (params.nonEmpty && params.head.isImplicit)
        (moreParams, params ::: implParams)
      else
        (params :: moreParams, implParams)
    case _ => (Nil, Nil)
  }

  def path(tree: Tree): String = tree match {
    case Select(prefix, name) => s"${path(prefix)}.$name"
    case _: Ident | _: This => tree.symbol.fullName
    case EmptyTree => "<none>"
    case _ => throw new IllegalArgumentException("This tree does not represent simple path: " + showRaw(tree))
  }

  /**
    * Is this tree a path that starts with package and goes through stable symbols (vals and objects)?
    *
    * @return
    */
  def isStableGlobalPath(tree: Tree): Boolean = tree match {
    case Select(prefix, _) => isStableTerm(tree.symbol) && isStableGlobalPath(prefix)
    case Ident(_) => tree.symbol.isStatic && isStableTerm(tree.symbol)
    case This(_) => tree.symbol.isPackageClass
    case _ => false
  }

  def isGlobalImplicitConversion(tree: Tree): Boolean = tree match {
    case TypeApply(prefix, _) => isGlobalImplicitConversion(prefix)
    //TODO handle apply method on implicit function values
    case Select(prefix, name) =>
      tree.symbol.isMethod && tree.symbol.isImplicit && isStableGlobalPath(prefix)
    case _ => false
  }

  // https://groups.google.com/forum/#!topic/scala-user/IeD2siVXyss
  def fixOverride(s: Symbol) =
    if (s.isTerm && s.asTerm.isOverloaded) {
      s.alternatives.filterNot(_.isSynthetic).head
    } else s

  def withOverrides(s: Symbol) =
    s :: s.overrides.map(fixOverride)

  def isStableStatic(symbol: Symbol) =
    symbol != null && symbol.isStatic && (symbol.isModule || symbol.isMethod && symbol.asMethod.isStable)

  def isFromToplevelType(symbol: Symbol) =
    withOverrides(symbol).exists(toplevelSymbols contains _.owner)

  def isJavaParameterlessMethod(symbol: Symbol) =
    symbol != null && symbol.isPublic && symbol.isJava && symbol.isMethod &&
      symbol.asMethod.paramLists == List(List()) && !symbol.typeSignature.takesTypeArgs

  def isJavaStaticType(tpe: Type) = {
    val symbol = tpe.typeSymbol
    symbol != null && symbol.isJava && isModuleOrPackage(symbol)
  }

  def isJavaClass(symbol: Symbol) =
    symbol.isJava && symbol.isClass && !symbol.isModuleClass && !symbol.isPackageClass

  def isStaticOrConstructor(symbol: Symbol) =
    symbol.isStatic || (symbol.isMethod && symbol.asMethod.isConstructor)

  def reifyOption[A](opt: Option[A], innerReify: A => Tree): Tree = opt match {
    case Some(x) => q"_root_.scala.Some(${innerReify(x)})"
    case None => q"_root_.scala.None"
  }

  def isBooleanType(tpe: Type) =
    tpe <:< booleanTpe || tpe <:< jBooleanTpe

  def isGetClass(symbol: Symbol) =
    symbol.name == TermName("getClass") && withOverrides(symbol).contains(getClassSymbol)

  def isBeanGetter(symbol: Symbol) = symbol.isMethod && {
    val methodSymbol = symbol.asMethod
    val name = symbol.name.decodedName.toString

    !isGetClass(methodSymbol) && methodSymbol.paramLists == List(List()) && methodSymbol.typeParams.isEmpty &&
      (BeanGetterNamePattern.pattern.matcher(name).matches ||
        BooleanBeanGetterNamePattern.pattern.matcher(name).matches && isBooleanType(methodSymbol.returnType))
  }

  def isParameterless(s: TermSymbol) =
    !s.isMethod || {
      val paramss = s.asMethod.paramLists
      paramss == Nil || paramss == List(Nil)
    }

  def methodTypesMatch(originalTpe: Type, implicitTpe: Type): Boolean = {
    def paramsMatch(origParams: List[Symbol], implParams: List[Symbol]): Boolean =
      (origParams, implParams) match {
        case (origHead :: origTail, implHead :: implTail) =>
          implHead.typeSignature <:< origHead.typeSignature && paramsMatch(origTail, implTail)
        case (Nil, Nil) => true
        case _ => false
      }

    (originalTpe, implicitTpe) match {
      case (MethodType(origParams, origResultType), MethodType(implParams, implResultType)) =>
        paramsMatch(origParams, implParams) && methodTypesMatch(origResultType, implResultType)
      case (MethodType(_, _), _) | (_, MethodType(_, _)) => false
      case (_, _) => true
    }
  }

  def takesSingleParameter(symbol: MethodSymbol) =
    symbol.paramLists match {
      case List(List(_)) => true
      case _ => false
    }

  def isBeanSetter(symbol: Symbol) =
    symbol.isMethod && {
      val methodSymbol = symbol.asMethod
      val name = symbol.name.decodedName.toString

      takesSingleParameter(methodSymbol) && methodSymbol.typeParams.isEmpty &&
        methodSymbol.returnType =:= typeOf[Unit] &&
        BeanSetterNamePattern.pattern.matcher(name).matches
    }

  /**
    * Accessible members include methods, modules, val/var setters and getters and Java fields.
    */
  def accessibleMembers(tpe: Type) =
    tpe.members.toList.collect { case s if s.isPublic && s.isTerm &&
      (s.isJava || (!s.asTerm.isVal && !s.asTerm.isVar)) && !s.isImplementationArtifact => s.asTerm
    }

  def hasType(tree: Tree, tpe: Type) =
    tree.tpe <:< tpe

  def toStringSymbol(tpe: Type) =
    symAlternatives(tpe.member(TermName("toString")))
      .find(s => s.isTerm && isParameterless(s.asTerm))
      .getOrElse(NoSymbol)

  def symAlternatives(sym: Symbol) = sym match {
    case termSymbol: TermSymbol => termSymbol.alternatives
    case NoSymbol => Nil
    case _ => List(sym)
  }

  def isExpressionUtil(symbol: Symbol): Boolean =
    symbol != null && symbol != NoSymbol &&
      (nonBottomSymbolType(symbol) <:< expressionUtilType || isExpressionUtil(symbol.owner))

  def isProfileObject(symbol: Symbol) =
    nonBottomSymbolType(symbol) <:< profileObjectType

  def isFromProfileObject(symbol: Symbol): Boolean =
    symbol != null && symbol != NoSymbol &&
      (isProfileObject(symbol) || isFromProfileObject(symbol.owner))

  def isScexSynthetic(symbol: Symbol): Boolean =
    symbol != null && symbol != NoSymbol &&
      (nonBottomSymbolType(symbol) <:< syntheticType || isScexSynthetic(symbol.owner))

  def isAdapter(tpe: Type): Boolean =
    tpe != null && !isBottom(tpe) && tpe <:< adapterType

  def isBottom(tpe: Type) =
    tpe <:< definitions.NullTpe || tpe <:< definitions.NothingTpe

  /**
    * Is this symbol the 'wrapped' field of Java getter adapter?
    */
  def isAdapterWrappedMember(symbol: Symbol): Boolean =
    if (symbol != null && symbol.isTerm) {
      val ts = symbol.asTerm
      if (ts.isGetter)
        ts.name == AdapterWrappedName && ts.owner.isType && isAdapter(ts.owner.asType.toType)
      else
        ts.isVal && ts.getter.isMethod && isAdapterWrappedMember(ts.getter)
    } else false

  def isRootAdapter(tpe: Type) =
    tpe != null && isAnnotatedWith(tpe.widen, rootAdapterAnnotType)

  def isAnnotatedWith(tpe: Type, annotTpe: Type): Boolean = tpe match {
    case AnnotatedType(annots, underlying) =>
      annots.exists(_.tree.tpe <:< annotTpe) || isAnnotatedWith(underlying, annotTpe)
    case ExistentialType(_, underlying) =>
      isAnnotatedWith(underlying, annotTpe)
    case _ => false
  }

  // gets Java getter called by implicit wrapper
  def getJavaGetter(symbol: Symbol, javaTpe: Type): Symbol = {
    val getterName = "get" + symbol.name.toString.capitalize
    val booleanGetterName = "is" + symbol.name.toString.capitalize

    def fail = throw new Exception(s"Could not find Java getter for property ${symbol.name} on $javaTpe")

    def findGetter(getterName: String) =
      symAlternatives(javaTpe.member(TermName(getterName))).find(isBeanGetter)

    if (isBooleanType(symbol.asMethod.returnType)) {
      findGetter(booleanGetterName) orElse findGetter(getterName) getOrElse fail
    } else {
      findGetter(getterName) getOrElse fail
    }
  }

  def symbolType(symbol: Symbol) =
    if (symbol == null) NoType
    else if (symbol.isType) symbol.asType.toType
    else symbol.typeSignature

  def nonBottomSymbolType(symbol: Symbol) = {
    val tpe = symbolType(symbol)
    if (tpe <:< definitions.NullTpe || tpe <:< definitions.NothingTpe) NoType else tpe
  }

  def isAdapterConversion(symbol: Symbol) =
    isProfileObject(symbol.owner) && symbol.isImplicit && symbol.isMethod && isAdapter(symbol.asMethod.returnType)

  def annotations(sym: Symbol): List[Annotation] = {
    sym.info // force annotations
    sym.annotations ++ (if (sym.isTerm) {
      val tsym = sym.asTerm
      if (tsym.isGetter) annotations(tsym.accessed) else Nil
    } else Nil)
  }

  def annotationsIncludingOverrides(sym: Symbol) =
    withOverrides(sym).flatMap(annotations)

  def debugTree(pref: String, tree: Tree): Unit = {
    println(pref)
    tree.foreach { t =>
      println(show(t.pos).padTo(15, ' ') + ("" + t.tpe).padTo(50, ' ') + show(t))
    }
    println()
  }
}

object MacroUtils {
  def apply(u: Universe) = new MacroUtils {
    val universe: u.type = u
  }
}
