package com.avsystem.scex.util

import com.avsystem.scex.util.CommonUtils._
import java.{util => ju, lang => jl}
import scala.Some
import scala.reflect.macros.Context

class MacroUtils[C <: Context with Singleton] private(val c: C) {

  import c.universe._
  import c.{Expr, reifyRuntimeClass}

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
      case TypeApply(prefix, _) =>
        unapply(prefix)
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

  def memberSignature(s: Symbol) =
    if (s != null) s"${s.fullName}:${s.typeSignature}" else null

  def path(tree: Tree): String = tree match {
    case TypeApply(prefix, _) => path(prefix)
    case Select(prefix, name) => s"${path(prefix)}.${name.decoded}"
    case Ident(name) => name.decoded
    case This(name) => name.decoded
    case EmptyTree => "<none>"
    case _ => throw new IllegalArgumentException("This tree does not represent simple path: " + showRaw(tree))
  }

  /**
   * Does this tree represent access to globally visible, stable identifier (i.e. a path from package through
   * objects and vals)?
   *
   * @param tree
   * @return
   */
  def refersToStableGlobalValue(tree: Tree): Boolean = tree.symbol.isPackage ||
    (tree match {
      case Select(prefix, name) =>
        tree.symbol.isTerm && tree.symbol.asTerm.isStable && refersToStableGlobalValue(prefix)
      case _ => false
    })

  def isValGetter(symbol: Symbol) =
    symbol.isMethod && symbol.asMethod.isGetter && {
      val accessed = symbol.asMethod.accessed
      accessed.isTerm && accessed.asTerm.isVal
    }

  def isGlobalImplicitConversion(tree: Tree): Boolean = tree match {
    case TypeApply(prefix, _) => isGlobalImplicitConversion(prefix)
    //TODO handle apply method on implicit function values
    case Select(prefix, name) => tree.symbol.isMethod && tree.symbol.isImplicit && refersToStableGlobalValue(prefix)
    case _ => false
  }

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

  def reifyRuntimeClassOpt(tpe: Type): Expr[Option[Class[_]]] =
    if (tpe == NoType || isJavaStaticType(tpe)) {
      reify(None)
    } else {
      reify(Some(Expr[Class[_]](reifyRuntimeClass(tpe)).splice))
    }

  def isBooleanType(tpe: Type) =
    tpe <:< typeOf[Boolean] || tpe <:< typeOf[jl.Boolean]

  def isBeanGetter(symbol: Symbol) = symbol.isMethod && {
    val methodSymbol = symbol.asMethod
    val name = symbol.name.decoded

    methodSymbol.paramss == List(List()) && methodSymbol.typeParams.isEmpty &&
      (BeanGetterNamePattern.pattern.matcher(name).matches ||
        BooleanBeanGetterNamePattern.pattern.matcher(name).matches && isBooleanType(methodSymbol.returnType))
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

  def wrapInFunction[T](expr: c.Expr[T]) = reify {
    def result = expr.splice
    result
  }

  def publicMethods(tpe: Type) =
    tpe.members.toList.collect { case s if s.isPublic && s.isMethod && !s.isImplementationArtifact => s.asMethod}

  def hasType[T: TypeTag](tree: Tree) =
    tree.tpe <:< typeOf[T]
}

object MacroUtils {
  def apply(c: Context) = new MacroUtils[c.type](c)
}