package com.avsystem.scex

import java.{util => ju, lang => jl}
import reflect.api.TypeCreator
import reflect.macros.Context
import scala.language.experimental.macros

object TestMacros {
  def lol[T]: TypeCreator = macro impl[T]

  def impl[T: c.WeakTypeTag](c: Context): c.Expr[TypeCreator] = {
    import c.universe._

    val Block(List(_, _), Apply(_, List(_, typeCreatorTree))) =
      c.reifyType(treeBuild.mkRuntimeUniverseRef, EmptyTree, weakTypeOf[T])

    c.Expr[TypeCreator](typeCreatorTree)
  }

  def gimme[T](expr: T) = macro gimme_impl[T]

  def gimme_impl[T](c: Context)(expr: c.Expr[T]) = {
    import c.universe._

    expr.tree foreach {
      tree => println(tree.tpe + " " + showRaw(tree))
    }

    expr
  }
}
