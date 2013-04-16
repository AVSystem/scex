package com.avsystem.scex

import java.{util => ju, lang => jl}
import scala.reflect.runtime.{universe => ru}
import com.avsystem.scex.validation.{SymbolValidator, SyntaxValidator}

class ExpressionProfile(
  val syntaxValidator: SyntaxValidator,
  val accessValidator: SymbolValidator,
  val wrappedJavaClasses: List[Class[_]],
  val expressionHeader: String)
