package com.avsystem.scex.compiler

import ExpressionCompilerConfig._
import java.{util => ju, lang => jl}
import scala.beans.BeanProperty
import scala.reflect.runtime.{universe => ru}


object ExpressionCompilerConfig {
  final val DEFAULT_EXPRESSION_EXPIRATION_TIME = 60 * 60 * 1000L
}

class ExpressionCompilerConfig {
  @BeanProperty var expressionExpirationTime = DEFAULT_EXPRESSION_EXPIRATION_TIME
}
