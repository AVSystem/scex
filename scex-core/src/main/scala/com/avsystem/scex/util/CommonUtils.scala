package com.avsystem.scex
package util

import java.util.concurrent.Callable


/**
 * Created with IntelliJ IDEA.
 * User: ghik
 * Date: 08.01.13
 * Time: 21:03
 */
object CommonUtils {

  val BeanGetterNamePattern = "get(([A-Z][a-z0-9_]*)+)".r
  val BooleanBeanGetterNamePattern = "is(([A-Z][a-z0-9_]*)+)".r
  val BeanSetterNamePattern = "set(([A-Z][a-z0-9_]*)+)".r

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

  def pluralize(count: Int, noun: String) =
    s"$count $noun" + (if (count != 1) "s" else "")

  def callable[T](expr: => T) =
    new Callable[T] {
      def call() = expr
    }
}
