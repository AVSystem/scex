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
     * Template expressions have form `part0&#36;{arg1}part1&#36;{arg2}part2...` where "parts" are plaintext
     * fragments (unquoted string literals) and "args" are arbitrary subexpressions.
     * <p/>
     * Template interpolation tries to concatenate `parts` and `args` according to following rules:
     * <ul>
     * <li>If expected result type is `String` or one of its supertypes (like `java.lang.Object` in particular),
     * parts and args are simply concatenated into a single string value. By default, args are converted to strings by
     * calling `toString()` on each one. This may be customized by the user. If you want your type, say `T`,
     * be spliced into the resulting string in some custom way, you can force this by introducing an implicit value of type
     * `TemplateInterpolations.Splicer[T]` into the expression scope (e.g. you can put it into expression utils).
     * </li>
     * <li>If expected result type is unrelated to `String`, all `parts` are empty strings and there is
     * only one argument to splice, that sole argument will simply be returned unchanged (except for possible implicit
     * conversions inferred by the compiler). As an convenience exception, if expected
     * result type is a Java enum and the type of spliced argument is `String`, that argument will be automatically
     * converted to enum value.</li>
     * <li>If the template expression consists of a single string literal (there are no arguments to splice), the compiler
     * will wrap it into an instance of [[com.avsystem.scex.util.Literal]]. Then, it will try to apply an implicit conversion
     * from `Literal` to the result type. By default, `Literal` will be automatically converted to
     * following types:
     * <ul>
     * <li>`Char`, `Boolean` and their `java.lang.*` boxed equivalents.
     * <li>numeric types: `Byte`, `Short`, `Int`, `Long`,
     * `Double`, `Float` and their `java.lang.*` boxed equivalents.</li>
     * <li>all Java enums</li>
     * </ul>
     * </li>
     * </ul>
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
  def concat(parts: String*)(args: Any*): String = {
    require(parts.size == args.size + 1)
    concatIterator(parts: _*)(args.iterator)
  }

  def concatIterator(parts: String*)(args: Iterator[Any]): String = {
    val sb = new StringBuilder(parts.head)
    (args zip parts.tail.iterator).foreach {
      case (arg, part) => sb.append(String.valueOf(arg)).append(part)
    }
    sb.result()
  }

}
