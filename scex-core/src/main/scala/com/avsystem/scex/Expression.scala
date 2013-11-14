package com.avsystem.scex

import java.{util => ju, lang => jl}
import scala.runtime.AbstractFunction1

trait Expression[-C <: ExpressionContext[_, _], +T] extends (C => T)

// just to look nicer in Java
abstract class AbstractExpression[-C <: ExpressionContext[_, _], +T] extends AbstractFunction1[C, T] with Expression[C, T]
