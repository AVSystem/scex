package com.avsystem.scex

import java.{util => ju, lang => jl}
import reflect.macros.Context
import scala.language.experimental.macros
import reflect.api.{TypeCreator, Universe}

object TestMacros {
  def lol[T]: TypeCreator = macro impl[T]

  def impl[T: c.WeakTypeTag](c: Context): c.Expr[TypeCreator] = {
    import c.universe._

    val Block(List(_, _), Apply(_, List(_, typeCreatorTree))) =
      c.reifyType(treeBuild.mkRuntimeUniverseRef, EmptyTree, weakTypeOf[T])

    c.Expr[TypeCreator](typeCreatorTree)
  }
}
