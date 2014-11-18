package com.avsystem.scex.symboldsl

import com.avsystem.scex.util.MacroUtils

import scala.collection.immutable.{SortedSet, TreeSet}
import scala.reflect.api.Universe

/**
 * Author: ghik
 * Created: 11/17/14.
 */
trait SymbolInfoList[T] {
  case class InfoWithIndex(info: SymbolInfo[T], index: Int)

  val infoList: List[SymbolInfo[T]]

  lazy val size = infoList.length

  lazy val bySignaturesMap: Map[String, List[InfoWithIndex]] =
    infoList.zipWithIndex.map {
      case (info, index) => InfoWithIndex(info, index)
    }.groupBy(_.info.memberSignature).map {
      case (signature, infos) => (signature, infos.sortBy(_.index))
    }.withDefaultValue(Nil)

  lazy val memberSignatures: SortedSet[String] =
    bySignaturesMap.keys.to[TreeSet]

  def matchingInfos(u: Universe)(prefixTpe: u.Type, symbol: u.Symbol, implicitConv: Option[u.Tree]): List[InfoWithIndex] = {
    val macroUtils = MacroUtils(u)
    import macroUtils._

    val signatures: List[String] =
      (symbol :: symbol.overrides).map(memberSignature)

    val implicitConvPath = implicitConv.map(path)

    signatures.flatMap { signature =>
      bySignaturesMap(signature).filter { case InfoWithIndex(symbolInfo, _) =>
        signature == symbolInfo.memberSignature &&
          prefixTpe <:< symbolInfo.typeInfo.typeIn(u) &&
          implicitConvPath == symbolInfo.implicitConv
      }
    }
  }
}
