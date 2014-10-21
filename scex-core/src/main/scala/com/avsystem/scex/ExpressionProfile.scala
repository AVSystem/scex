package com.avsystem.scex

import java.{lang => jl, util => ju}

import com.avsystem.scex.validation.{SymbolValidator, SyntaxValidator}

class ExpressionProfile(
  val name: String,
  val syntaxValidator: SyntaxValidator,
  val symbolValidator: SymbolValidator,
  val expressionHeader: String,
  val expressionUtils: String) {

  override def toString = s"ExpressionProfile[$name]"
}