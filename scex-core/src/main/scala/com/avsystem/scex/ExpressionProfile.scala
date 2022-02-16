package com.avsystem.scex

import com.avsystem.commons.annotation.bincompat
import com.avsystem.scex.presentation.SymbolAttributes
import com.avsystem.scex.validation.{SymbolValidator, SyntaxValidator}

class ExpressionProfile(
  val name: String,
  val syntaxValidator: SyntaxValidator,
  val symbolValidator: SymbolValidator,
  val symbolAttributes: SymbolAttributes,
  val expressionHeader: String,
  val expressionUtils: NamedSource,
  val dynamicVariablesEnabled: Boolean) {

  @bincompat
  def this(
    name: String,
    syntaxValidator: SyntaxValidator,
    symbolValidator: SymbolValidator,
    symbolAttributes: SymbolAttributes,
    expressionHeader: String,
    expressionUtils: NamedSource
  ) = this(name, syntaxValidator, symbolValidator, symbolAttributes, expressionHeader, expressionUtils, dynamicVariablesEnabled = true)

  override def toString = s"ExpressionProfile[$name]"
}
