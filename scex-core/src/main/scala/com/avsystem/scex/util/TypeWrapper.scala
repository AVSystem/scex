package com.avsystem.scex.util

import java.util.Objects
import java.{lang => jl, util => ju}

import scala.collection.mutable.ListBuffer
import scala.reflect.api.Universe

/**
 * The purpose of this class is to have meaningful equals/hashCode semantics for Type objects
 */
class TypeWrapper(universeWithTpe: (u.type, u.Type) forSome {val u: Universe}) {
  private val u = universeWithTpe._1
  private val tpe = universeWithTpe._2.asInstanceOf[u.Type]

  import u._

  private def getSymbols(tpe: Type): List[Symbol] = {
    val b = new ListBuffer[Symbol]

    def symbolsIn(tpe: Type): Unit = tpe match {
      case SingleType(pre, sym) =>
        b += sym
        symbolsIn(pre)
      case TypeRef(pre, sym, args) =>
        b += sym
        symbolsIn(pre)
        args.foreach(symbolsIn)
      case _ =>
        b += tpe.typeSymbol
    }
    symbolsIn(tpe)
    b.result()
  }

  private val symbols = getSymbols(tpe)

  override def equals(other: Any) = other match {
    case other: TypeWrapper =>
      (u eq other.u) && symbols == other.symbols && tpe =:= other.tpe.asInstanceOf[Type]
    case _ => false
  }

  override val hashCode = Objects.hash(u, symbols)
}

object TypeWrapper {
  def apply(u: Universe)(tpe: u.Type) = new TypeWrapper((u, tpe): (u.type, u.Type))
}