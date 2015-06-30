package com.avsystem.scex

import java.{lang => jl, util => ju}

import com.avsystem.scex.TestMacros.SymbolStringDsl
import com.avsystem.scex.symboldsl.{SymbolDsl, SymbolInfo, SymbolInfoParser}

import scala.language.experimental.macros
import scala.reflect.api.{TreeCreator, TypeCreator}
import scala.reflect.macros.blackbox

object TestMacros {
  def lol[T]: TypeCreator = macro TestMacros.impl[T]

  def gimme[T](expr: T): (TreeCreator, TypeCreator) = macro TestMacros.gimme_impl[T]

  object SymbolStringDsl extends SymbolDsl {
    type Payload = String

    def strings(any: Any): List[SymbolInfo[String]] = macro TestMacros.strings_impl
  }
}

class TestMacros(val c: blackbox.Context) {
  import c.universe._

  def impl[T: c.WeakTypeTag]: c.Expr[TypeCreator] = {

    val Block(List(_, _), Apply(_, List(_, typeCreatorTree))) =
      c.reifyType(internal.gen.mkRuntimeUniverseRef, EmptyTree, weakTypeOf[T])

    c.Expr[TypeCreator](typeCreatorTree)
  }

  def gimme_impl[T](expr: c.Expr[T]): c.Expr[(TreeCreator, TypeCreator)] = {

    val reifiedTree = c.reifyTree(internal.gen.mkRuntimeUniverseRef, EmptyTree, expr.tree)
    val Block(List(_, _), Apply(Apply(_, List(_, treeCreatorTree)), List(Apply(_, List(_, typeCreatorTree))))) = reifiedTree

    reify {
      (c.Expr[TreeCreator](treeCreatorTree).splice, c.Expr[TypeCreator](typeCreatorTree).splice)
    }
  }

  def strings_impl(any: c.Expr[Any]): c.Tree = {
    SymbolInfoParser(SymbolStringDsl, c)(q"${""}").extractSymbolInfos(any.tree)
  }

}
