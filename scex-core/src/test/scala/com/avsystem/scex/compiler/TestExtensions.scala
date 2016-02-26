package com.avsystem.scex.compiler

/**
 * Author: ghik
 * Created: 19/10/15.
 */
object TestExtensions {
  implicit class any2qmark[A](a: => A) {
    def ?[B >: A](default: => B) = {
      val ar = a
      if (ar == null) default else ar
    }
  }
}
