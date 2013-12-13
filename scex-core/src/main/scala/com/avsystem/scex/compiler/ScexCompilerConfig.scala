package com.avsystem.scex
package compiler

import ScexCompilerConfig._
import java.{util => ju, lang => jl}
import scala.beans.BeanProperty


object ScexCompilerConfig {
  final val DEFAULT_EXPRESSION_EXPIRATION_TIME = 60 * 60 * 1000L
  final val DEFAULT_COMPLETION_EXPIRATION_TIME = 10 * 60 * 1000L
  final val DEFAULT_RESET_AFTER_COMPILATION_COUNT = 2000
}

class ScexCompilerConfig(
  @BeanProperty var expressionExpirationTime: Long,
  @BeanProperty var completionExpirationTime: Long,
  @BeanProperty var resetAfterCompilationCount: Long) {

  def this() = this(
    DEFAULT_EXPRESSION_EXPIRATION_TIME,
    DEFAULT_COMPLETION_EXPIRATION_TIME,
    DEFAULT_RESET_AFTER_COMPILATION_COUNT)
}
