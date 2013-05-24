package com.avsystem.scex.compiler

import com.avsystem.scex.validation.{SymbolValidator, SyntaxValidator}
import java.{util => ju, lang => jl}


class ExpressionProfile(
  val syntaxValidator: SyntaxValidator,
  val symbolValidator: SymbolValidator,
  val expressionHeader: String)
