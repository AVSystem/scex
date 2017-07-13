package com.avsystem.scex.util

/**
  * If some exception extends this trait, it may be caught and recovered from using `?` operator in expressions.
  * See [[CommonExpressionUtils.any2qmark.?]]
  */
trait ExpressionRecoverableException extends Throwable
