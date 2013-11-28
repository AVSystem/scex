package com.avsystem.scex
package util

import com.avsystem.scex.compiler.annotation._
import com.avsystem.scex.util.CommonUtils._
import java.{util => ju, lang => jl}
import scala.Some
import scala.reflect.macros.Universe
import scala.runtime.StringAdd

trait MacroUtils {
  val universe: Universe

  import universe._

  lazy val adapterAnnotType = typeOf[JavaGetterAdapter]
  lazy val adapterConversionAnnotType = typeOf[JavaGetterAdapterConversion]
  lazy val booleanGetterAnnotType = typeOf[BooleanIsGetter]
  lazy val rootAdapterAnnotType = typeOf[RootAdapter]
  lazy val expressionUtilAnnotType = typeOf[ExpressionUtil]
  lazy val profileObjectAnnotType = typeOf[ProfileObject]
  lazy val wrappedInAdapterAnnotType = typeOf[WrappedInAdapter]
  lazy val notValidatedAnnotType = typeOf[NotValidated]

  lazy val any2stringadd = typeOf[Predef.type].member(newTermName("any2stringadd"))
  lazy val stringAddPlus = typeOf[StringAdd].member(newTermName("+").encodedName)
  lazy val stringConcat = typeOf[String].member(newTermName("+").encodedName)
  lazy val stringTpe = typeOf[String]
  lazy val booleanTpe = typeOf[Boolean]
  lazy val jBooleanTpe = typeOf[jl.Boolean]

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
        if isGlobalImplicitConversion(fun) && (tree.pos == NoPosition || tree.pos == prefix.pos) =>
        Some((prefix, fun))
      case _ =>
        None
    }
  }

  object NewInstance {
    def unapply(tree: Tree) = tree match {
      case Apply(Select(New(tpeTree), nme.CONSTRUCTOR), args) =>
        Some((tpeTree, args))
      case _ =>
        None
    }
  }

  object TermName {
    def unapply(termName: TermName) = Some(termName.decoded)
  }

  def isModuleOrPackage(symbol: Symbol) = symbol != null &&
    (symbol.isModule || symbol.isModuleClass || symbol.isPackage || symbol.isPackageClass)

  def isJavaField(symbol: Symbol) =
    symbol != null && symbol.isJava && symbol.isTerm && !symbol.isMethod && !isModuleOrPackage(symbol)

  def isConstructor(s: Symbol) =
    s.isMethod && s.asMethod.isConstructor

  def memberSignature(s: Symbol) =
    if (s != null) s"${s.fullName}:${s.typeSignature}" else null

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

  def path(tree: Tree): String = tree match {
    case Select(prefix, name) => s"${path(prefix)}.${name.decoded}"
    case Ident(name) => name.decoded
    case This(name) => name.decoded
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

  lazy val toplevelSymbols = Set(typeOf[Any], typeOf[AnyRef], typeOf[AnyVal]).map(_.typeSymbol)

  def isStaticModule(symbol: Symbol) =
    symbol != null && symbol.isModule && symbol.isStatic

  def isFromToplevelType(symbol: Symbol) =
    (symbol :: symbol.allOverriddenSymbols).exists(toplevelSymbols contains _.owner)

  def isJavaParameterlessMethod(symbol: Symbol) =
    symbol != null && symbol.isPublic && symbol.isJava && symbol.isMethod &&
      symbol.asMethod.paramss == List(List()) && !symbol.typeSignature.takesTypeArgs

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

  lazy val getClassSymbol = typeOf[Any].member(newTermName("getClass"))

  def isGetClass(symbol: Symbol) =
    symbol.name == newTermName("getClass") && (symbol :: symbol.allOverriddenSymbols).contains(getClassSymbol)

  def isBeanGetter(symbol: Symbol) = symbol.isMethod && {
    val methodSymbol = symbol.asMethod
    val name = symbol.name.decoded

    !isGetClass(methodSymbol) && methodSymbol.paramss == List(List()) && methodSymbol.typeParams.isEmpty &&
      (BeanGetterNamePattern.pattern.matcher(name).matches ||
        BooleanBeanGetterNamePattern.pattern.matcher(name).matches && isBooleanType(methodSymbol.returnType))
  }

  def isParameterless(s: TermSymbol) =
    !s.isMethod || {
      val paramss = s.asMethod.paramss
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
    symbol.paramss match {
      case List(List(_)) => true
      case _ => false
    }

  def isBeanSetter(symbol: Symbol) =
    symbol.isMethod && {
      val methodSymbol = symbol.asMethod
      val name = symbol.name.decoded

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
    tpe.member(newTermName("toString")).asTerm.alternatives.find(s => s.isTerm && isParameterless(s.asTerm)).get

  lazy val standardStringInterpolations =
    Set("s", "raw").map(name => typeOf[StringContext].member(newTermName(name)))

  def alternatives(sym: Symbol) = sym match {
    case termSymbol: TermSymbol => termSymbol.alternatives
    case NoSymbol => Nil
    case _ => List(sym)
  }

  def isExpressionUtil(symbol: Symbol): Boolean =
    symbol != null && symbol != NoSymbol &&
      (isExpressionUtilObject(symbol) || isExpressionUtil(symbol.owner))

  def isExpressionUtilObject(symbol: Symbol): Boolean =
    symbol != null && symbol != NoSymbol && symbol.annotations.exists(_.tpe =:= expressionUtilAnnotType)

  def isProfileObject(symbol: Symbol) =
    symbol != null && symbol.annotations.exists(_.tpe =:= profileObjectAnnotType)

  def isScexSynthetic(symbol: Symbol): Boolean =
    symbol != null && symbol != NoSymbol &&
      (isProfileObject(symbol) || isScexSynthetic(symbol.owner))

  def isAdapter(symbol: Symbol): Boolean =
    symbol != NoSymbol && symbol.annotations.exists(_.tpe =:= adapterAnnotType)

  def isAdapter(tpe: Type): Boolean =
    tpe != NoType && isAdapter(tpe.typeSymbol)

  /**
   * Is this symbol the 'wrapped' field of Java getter adapter?
   */
  def isAdapterWrappedMember(symbol: Symbol): Boolean =
    symbol != NoSymbol && symbol.annotations.exists(_.tpe =:= wrappedInAdapterAnnotType)

  def isRootAdapter(symbol: Symbol) =
    symbol.annotations.exists(_.tpe =:= rootAdapterAnnotType)

  def isBooleanGetterAdapter(symbol: Symbol) =
    symbol.annotations.exists(_.tpe =:= booleanGetterAnnotType)

  // gets Java getter called by implicit wrapper
  def getJavaGetter(symbol: Symbol, javaTpe: Type): Symbol = {
    val prefix = if (isBooleanGetterAdapter(symbol)) "is" else "get"
    val name = prefix + symbol.name.toString.capitalize

    def fail = throw new Error(s"Could not find Java getter $name on $javaTpe")
    alternatives(javaTpe.member(newTermName(name))).find(isBeanGetter).getOrElse(fail)
  }
}

object MacroUtils {
  def apply(u: Universe) = new MacroUtils {
    val universe: u.type = u
  }
}
