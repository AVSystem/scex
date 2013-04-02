package com.avsystem.scex.validation

import java.{util => ju, lang => jl}
import scala.reflect.macros.Context
import scala.language.experimental.macros

object Validator {
  def validate[T >: Null](expr: T): T = macro validate_impl[T]

  def validate_impl[T >: Null](c: Context)(expr: c.Expr[T]): c.Expr[T] = {
    import c.universe._

    expr.tree.collect({
      case tree => (tree.symbol, tree.tpe, showRaw(tree))
    }) foreach println

    println("STUFF: " + showRaw(reify(reify(null))))

    expr
  }
}
