package com.avsystem.scex

import java.{util => ju, lang => jl}
import scala.collection.mutable
import scala.collection.JavaConverters._
import scala.reflect.runtime.{universe => ru}
import validation.{AccessValidator, SyntaxValidator}

class ExpressionProfile(
  val syntaxValidator: SyntaxValidator,
  val accessValidator: AccessValidator,
  val wrappedJavaClasses: List[Class[_]],
  val expressionHeader: String)
