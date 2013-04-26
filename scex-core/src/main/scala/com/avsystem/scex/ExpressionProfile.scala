package com.avsystem.scex

import com.avsystem.scex.validation.{SymbolValidator, SyntaxValidator}
import java.{util => ju, lang => jl}
import scala.reflect.runtime.{universe => ru}

class ExpressionProfile(
  val syntaxValidator: SyntaxValidator,
  val accessValidator: SymbolValidator,
  val expressionHeader: String)
