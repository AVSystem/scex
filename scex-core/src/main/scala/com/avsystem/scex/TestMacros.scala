package com.avsystem.scex

import java.{util => ju, lang => jl}
import reflect.macros.whitebox.Context
import scala.language.experimental.macros
import scala.reflect.api.{TreeCreator, TypeCreator}

object TestMacros {
  def lol[T]: TypeCreator = macro impl[T]

  def impl[T: c.WeakTypeTag](c: Context): c.Expr[TypeCreator] = {
    import c.universe._

    val Block(List(_, _), Apply(_, List(_, typeCreatorTree))) =
      c.reifyType(internal.gen.mkRuntimeUniverseRef, EmptyTree, weakTypeOf[T])

    c.Expr[TypeCreator](typeCreatorTree)
  }

  def gimme[T](expr: T): (TreeCreator, TypeCreator) = macro gimme_impl[T]

  def gimme_impl[T](c: Context)(expr: c.Expr[T]): c.Expr[(TreeCreator, TypeCreator)] = {
    import c.universe._

    val reifiedTree = c.reifyTree(internal.gen.mkRuntimeUniverseRef, EmptyTree, expr.tree)
    val Block(List(_, _), Apply(Apply(_, List(_, treeCreatorTree)), List(Apply(_, List(_, typeCreatorTree))))) = reifiedTree

    reify {
      (c.Expr[TreeCreator](treeCreatorTree).splice, c.Expr[TypeCreator](typeCreatorTree).splice)
    }
  }
}
