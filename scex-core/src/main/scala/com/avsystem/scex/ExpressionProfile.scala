package com.avsystem.scex

import com.avsystem.scex.validation.{SymbolValidator, SyntaxValidator}
import java.{util => ju, lang => jl}

class ExpressionProfile(
  val syntaxValidator: SyntaxValidator,
  val symbolValidator: SymbolValidator,
  val expressionHeader: String,
  val expressionUtils: String)