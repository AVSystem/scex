package com.avsystem.scex.util

import com.avsystem.scex.util.CommonUtils._
import java.{util => ju, lang => jl}
import scala.Some
import scala.reflect.macros.Context

class MacroUtils[C <: Context with Singleton](val context: C) {

  import context.{Expr, reifyRuntimeClass}
  import context.universe._

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
    def unapply(tree: Tree) = tree match {
      case Apply(fun, List(prefix)) if isStaticImplicitConversion(fun.symbol) && tree.pos == prefix.pos =>
        Some((prefix, fun))
      case _ =>
        None
    }
  }

  def isModuleOrPackage(symbol: Symbol) = symbol != null &&
    (symbol.isModule || symbol.isModuleClass || symbol.isPackage || symbol.isPackageClass)

  def isJavaField(symbol: Symbol) =
    symbol != null && symbol.isJava && symbol.isTerm && !symbol.isMethod && !isModuleOrPackage(symbol)

  def memberSignature(s: Symbol) =
    if (s != null) s"${s.fullName}:${s.typeSignature}" else null

  def isStaticImplicitConversion(symbol: Symbol) =
    symbol != null && symbol.isMethod && symbol.isStatic && symbol.isImplicit

  def isJavaParameterlessMethod(symbol: Symbol) =
    symbol != null && symbol.isPublic && symbol.isJava && symbol.isMethod &&
      symbol.asMethod.paramss == List(List()) && !symbol.typeSignature.takesTypeArgs

  def isJavaStaticType(tpe: Type) = {
    val symbol = tpe.typeSymbol
    symbol != null && symbol.isJava && isModuleOrPackage(symbol)
  }

  def isStaticOrConstructor(symbol: Symbol) =
    symbol.isStatic || (symbol.isMethod && symbol.asMethod.isConstructor)

  def reifyOption[A, B](opt: Option[A], innerReify: A => Expr[B]): Expr[Option[B]] = opt match {
    case Some(x) => reify(Some(innerReify(x).splice))
    case None => reify(None)
  }

  def reifyRuntimeClassOpt(tpe: Type): Expr[Option[Class[_]]] =
    if (isJavaStaticType(tpe)) {
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
}

object MacroUtils {
  def apply(c: Context) = new MacroUtils[c.type](c)
}