package com.avsystem.scex.util

import com.avsystem.commons.jiop.JavaInterop._

final class EnrichedArray[T](private val wrapped: Array[T]) extends AnyVal {
  def asList: JList[T] = java.util.Arrays.asList(wrapped: _*)
  def get(index: Int): T = wrapped(index)
}
