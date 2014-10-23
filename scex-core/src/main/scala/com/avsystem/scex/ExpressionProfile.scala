package com.avsystem.scex

import java.{lang => jl, util => ju}

import com.avsystem.scex.util.CommonUtils._
import com.avsystem.scex.validation.{SymbolValidator, SyntaxValidator}

class ExpressionProfile(
  val name: String,
  val syntaxValidator: SyntaxValidator,
  val symbolValidator: SymbolValidator,
  val expressionHeader: String,
  val expressionUtils: String) {

  require(name.isAlphaNumeric, s"Name must contain only alphanumeric characters, got $name")

  override def toString = s"ExpressionProfile[$name]"
}
