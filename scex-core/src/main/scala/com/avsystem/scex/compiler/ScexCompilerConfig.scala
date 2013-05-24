package com.avsystem.scex.compiler

import ScexCompilerConfig._
import java.{util => ju, lang => jl}
import scala.beans.BeanProperty


object ScexCompilerConfig {
  final val DEFAULT_EXPRESSION_EXPIRATION_TIME = 60 * 60 * 1000L
  final val DEFAULT_RESET_AFTER_COMPILATION_COUNT = 2000
}

class ScexCompilerConfig {
  @BeanProperty var expressionExpirationTime = DEFAULT_EXPRESSION_EXPIRATION_TIME
  @BeanProperty var resetAfterCompilationCount = DEFAULT_RESET_AFTER_COMPILATION_COUNT
}
