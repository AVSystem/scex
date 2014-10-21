package com.avsystem.scex

import java.{lang => jl, util => ju}

import com.avsystem.scex.validation.{SymbolValidator, SyntaxValidator}

class ExpressionProfile(
  val name: String,
  val syntaxValidator: SyntaxValidator,
  val symbolValidator: SymbolValidator,
  val expressionHeader: String,
  val expressionUtils: String) {

  require(name.matches("[a-zA-Z0-9]+"), s"Invalid profile name $name, only alphanumeric characters are allowed")

  override def toString = s"ExpressionProfile[$name]"
}