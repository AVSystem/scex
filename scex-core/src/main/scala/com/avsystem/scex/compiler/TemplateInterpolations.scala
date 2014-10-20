package com.avsystem.scex
package compiler

import java.{lang => jl, util => ju}

import com.avsystem.scex.compiler.annotation.NotValidated

import scala.language.experimental.macros

/**
 * Created: 18-11-2013
 * Author: ghik
 */
trait TemplateInterpolations[T] {

  implicit class TemplateInterpolation(sc: StringContext) {
    /**
     * String interpolation used to compile <i>template</i> expressions.
     * Template expressions have form <code>part0${arg1}part1${arg2}part2...</code> where "parts" are plaintext
     * fragments (unquoted string literals) and "args" are arbitrary subexpressions.
     * <p/>
     * Template interpolation tries to concatenate <code>parts</code> and <code>args</code> according to following rules:
     * <ul>
     * <li>If expected result type is <code>String</code> or one of its supertypes (like <code>java.lang.Object</code> in particular),
     * parts and args are simply concatenated into a single string value. By default, args are converted to strings by
     * calling <code>toString()</code> on each one. This may be customized by the user. If you want your type, say <code>T</code>,
     * be spliced into the resulting string in some custom way, you can force this by introducing an implicit value of type
     * <code>TemplateInterpolations.Splicer[T]</code> into the expression scope (e.g. you can put it into expression utils).
     * </li>
     * <li>If expected result type is unrelated to <code>String</code>, all <code>parts</code> are empty strings and there is
     * only one argument to splice, that sole argument will simply be returned unchanged (except for possible implicit
     * conversions inferred by the compiler). As an convenience exception, if expected
     * result type is a Java enum and the type of spliced argument is <code>String</code>, that argument will be automatically
     * converted to enum value.</li>
     * <li>If the template expression consists of a single string literal (there are no arguments to splice), the compiler
     * will wrap it into an instance of {@link com.avsystem.scex.util.Literal}. Then, it will try to apply an implicit conversion
     * from <code>Literal</code> to the result type. By default, <code>Literal</code> will be automatically converted to
     * following types:
     * <ul>
     * <li><code>Char</code>, <ode>Boolean</code> and their <code>java.lang.*</code> boxed equivalents.
     * <li>numeric types: <code>Byte</code>, <code>Short</code>, <code>Int</code>, <code>Long</code>,
     * <code>Double</code>, <code>Float</code> and their <code>java.lang.*</code> boxed equivalents.</li>
     * <li>all Java enums</li>
     * </ul>
     * </li>
     * </ul>
     * @param args
     * @return
     */
    def t[A](args: A*): T = macro Macros.templateInterpolation_impl[T, A]
  }

}

object TemplateInterpolations {

  trait Splicer[T] {
    def toString(t: T): String
  }

  @NotValidated
  def concat(parts: String*)(args: Any*) = {
    require(parts.size == args.size + 1)
    val sb = new StringBuilder(parts.head)
    (args zip parts.tail).foreach {
      case (arg, part) => sb.append(arg).append(part)
    }
    sb.result()
  }

}
