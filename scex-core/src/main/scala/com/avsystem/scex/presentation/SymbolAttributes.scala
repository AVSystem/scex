package com.avsystem.scex.presentation

import com.avsystem.scex.symboldsl.{SymbolDsl, SymbolDslMacros, SymbolInfo, SymbolInfoList}

import scala.language.experimental.macros

/**
 * Author: ghik
 * Created: 11/17/14.
 */
class SymbolAttributes(val infoList: List[SymbolInfo[Attributes]]) extends SymbolInfoList[Attributes]

object SymbolAttributes extends SymbolDsl {
  type Payload = Attributes

  def apply(attrList: List[SymbolInfo[Attributes]]): SymbolAttributes =
    new SymbolAttributes(attrList)

  val empty: SymbolAttributes = apply(Nil)

  def attributes(any: Any): List[SymbolInfo[Attributes]] = macro SymbolDslMacros.attributes_impl
}
