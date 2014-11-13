package com.avsystem.scex
package util

import com.avsystem.scex.compiler.annotation._
import com.avsystem.scex.util.CommonUtils._
import com.avsystem.scex.validation.FakeImplicitConversion
import java.{util => ju, lang => jl}
import scala.reflect.macros.Universe
import com.avsystem.scex.compiler.Markers.{Synthetic, ProfileObject, ExpressionUtil, JavaGetterAdapter}

trait MacroUtils {
  val universe: Universe

  import universe._

  lazy val adapterType = typeOf[JavaGetterAdapter]
  lazy val syntheticType = typeOf[Synthetic]
  lazy val expressionUtilType = typeOf[ExpressionUtil]
  lazy val profileObjectType = typeOf[ProfileObject]

  lazy val rootAdapterAnnotType = typeOf[RootAdapter]
  lazy val notValidatedAnnotType = typeOf[NotValidated]
  lazy val inputAnnotType = typeOf[Input]

  lazy val any2stringadd = typeOf[Predef.type].member(TermName("any2stringadd"))
  lazy val stringAddPlus = typeOf[any2stringadd[_]].member(TermName("+").encodedName)
  lazy val stringConcat = typeOf[String].member(TermName("+").encodedName)
  lazy val stringTpe = typeOf[String]
  lazy val booleanTpe = typeOf[Boolean]
  lazy val jBooleanTpe = typeOf[jl.Boolean]
  lazy val dynamicTpe = typeOf[Dynamic]

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

  def paramsOf(tpe: Type): List[List[Symbol]] = tpe match {
    case MethodType(params, resultType) =>
      params :: paramsOf(resultType)
    case _ => Nil
  }

  def resultTypeOf(tpe: Type): Type = tpe match {
    case tpe: MethodType => resultTypeOf(tpe.resultType)
    case tpe: NullaryMethodType => tpe.resultType
    case _ => tpe
  }

  private object FakeImplicitConversionTree {
    def unapply(tree: Tree) = internal.attachments(tree).get[FakeImplicitConversion] match {
      case Some(FakeImplicitConversion(fakePath)) => Some(fakePath)
      case None => None
    }
  }

  def path(tree: Tree): String = tree match {
    case FakeImplicitConversionTree(fakePath) => fakePath
    case Select(prefix, name) => s"${path(prefix)}.${name.decodedName.toString}"
    case Ident(name) => name.decodedName.toString
    case This(name) => name.decodedName.toString
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
    case FakeImplicitConversionTree(_) => true
    case TypeApply(prefix, _) => isGlobalImplicitConversion(prefix)
    //TODO handle apply method on implicit function values
    case Select(prefix, name) =>
      tree.symbol.isMethod && tree.symbol.isImplicit && isStableGlobalPath(prefix)
    case _ => false
  }

  lazy val toplevelSymbols = Set(typeOf[Any], typeOf[AnyRef], typeOf[AnyVal]).map(_.typeSymbol)

  def isStaticModule(symbol: Symbol) =
    symbol != null && symbol.isModule && symbol.isStatic

  def isFromToplevelType(symbol: Symbol) =
    (symbol :: symbol.overrides).exists(toplevelSymbols contains _.owner)

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

  def reifyOption[A, B](opt: Option[A], innerReify: A => Expr[B]): Expr[Option[B]] = opt match {
    case Some(x) => reify(Some(innerReify(x).splice))
    case None => reify(None)
  }

  def isBooleanType(tpe: Type) =
    tpe <:< typeOf[Boolean] || tpe <:< typeOf[jl.Boolean]

  lazy val getClassSymbol = typeOf[Any].member(TermName("getClass"))

  def isGetClass(symbol: Symbol) =
    symbol.name == TermName("getClass") && (symbol :: symbol.overrides).contains(getClassSymbol)

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

  def hasType[T: TypeTag](tree: Tree) =
    tree.tpe <:< typeOf[T]

  def toStringSymbol(tpe: Type) =
    alternatives(tpe.member(TermName("toString")))
      .find(s => s.isTerm && isParameterless(s.asTerm))
      .getOrElse(NoSymbol)

  lazy val standardStringInterpolations =
    Set("s", "raw").map(name => typeOf[StringContext].member(TermName(name)))

  def alternatives(sym: Symbol) = sym match {
    case termSymbol: TermSymbol => termSymbol.alternatives
    case NoSymbol => Nil
    case _ => List(sym)
  }

  def isExpressionUtil(symbol: Symbol): Boolean =
    symbol != null && symbol != NoSymbol &&
      (isExpressionUtilObject(symbol) || isExpressionUtil(symbol.owner))

  def isExpressionUtilObject(symbol: Symbol): Boolean =
    nonBottomSymbolType(symbol) <:< expressionUtilType

  def isProfileObject(symbol: Symbol) =
    nonBottomSymbolType(symbol) <:< profileObjectType

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
      (ts.isGetter && ts.name == TermName("_wrapped") && ts.owner.isType && isAdapter(ts.owner.asType.toType)
        || ts.isVal && isAdapterWrappedMember(ts.getter))
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
      alternatives(javaTpe.member(TermName(getterName))).find(isBeanGetter)

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
