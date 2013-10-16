package com.avsystem.scex.japi

import java.{util => ju, lang => jl}

/**
 * Created: 16-10-2013
 * Author: ghik
 */
abstract class ExpressionContext[R, V] extends com.avsystem.scex.ExpressionContext {
  type Root = R
  type Variable = V
}