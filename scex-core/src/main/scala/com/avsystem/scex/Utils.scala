package com.avsystem.scex

import scala.reflect.api.Universe
import scala.reflect.macros.Context
import scala.collection.mutable

/**
 * Created with IntelliJ IDEA.
 * User: ghik
 * Date: 08.01.13
 * Time: 21:03
 */
object Utils {

  implicit class EnhancedInt(val i: Int) extends AnyVal {
    def times(expr: => Any) {
      var c = 0
      while (c < i) {
        expr
        c += 1
      }
    }
  }

  implicit class EnhancedString(val str: String) extends AnyVal {
    def leftPad(w: Int) =
      if (str.length >= w)
        str.substring(0, w)
      else {
        str + " " * (str.length - w)
      }
  }

  def benchmark(expr: => Any): Double = {
    val start = System.nanoTime()
    expr
    (System.nanoTime() - start) / 1000000000.0
  }

  def hierarchy(clazz: Class[_]): Set[Class[_]] = {
    val resultBuilder = Set.newBuilder[Class[_]]

    def fill(clazz: Class[_]) {
      if (clazz != null) {
        resultBuilder += clazz
        fill(clazz.getSuperclass)
        clazz.getInterfaces.foreach(fill)
      }
    }

    fill(clazz)
    resultBuilder.result()
  }

  def isModuleOrPackage(symbol: Universe#Symbol) = symbol != null &&
    (symbol.isModule || symbol.isModuleClass || symbol.isPackage || symbol.isPackageClass)

  def isJavaField(symbol: Universe#Symbol) =
    symbol != null && symbol.isJava && symbol.isTerm && !symbol.isMethod && !isModuleOrPackage(symbol)

  def memberSignature(s: Universe#Symbol) =
    if (s != null) s"${s.fullName}:${s.typeSignature}" else null

  def isStaticImplicitConversion(symbol: Universe#Symbol) =
    symbol != null && symbol.isMethod && symbol.isStatic && symbol.isImplicit

  def isJavaParameterlessMethod(symbol: Universe#Symbol) =
    symbol != null && symbol.isPublic && symbol.isJava && symbol.isMethod &&
      symbol.asMethod.paramss == List(List()) && !symbol.typeSignature.takesTypeArgs

  def isJavaStaticType(tpe: Universe#Type) = {
    val symbol = tpe.typeSymbol
    symbol != null && symbol.isJava && isModuleOrPackage(symbol)
  }

  def isStaticOrConstructor(symbol: Universe#Symbol) =
    symbol.isStatic || (symbol.isMethod && symbol.asMethod.isConstructor)

  def reifyOption[A, B](c: Context)(opt: Option[A], innerReify: A => c.Expr[B]): c.Expr[Option[B]] = opt match {
    case Some(x) => c.universe.reify(Some(innerReify(x).splice))
    case None => c.universe.reify(None)
  }

  def reifyRuntimeClassOpt(c: Context)(tpe: c.universe.Type) =
    if (isJavaStaticType(tpe)) {
      c.universe.reify(None)
    } else {
      c.universe.reify(Some(c.Expr[Class[_]](c.reifyRuntimeClass(tpe)).splice))
    }

  def memberThat(u: Universe)(tpe: u.Type, name: String, predicate: u.Symbol => Boolean): u.Symbol = {
    import u._
    tpe.member(u.newTermName(name)) match {
      case symbol if symbol.isTerm => symbol.asTerm.alternatives.find(predicate).getOrElse(NoSymbol)
      case symbol if predicate(symbol) => symbol
      case _ => NoSymbol
    }
  }
}
