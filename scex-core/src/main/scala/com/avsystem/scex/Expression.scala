package com.avsystem.scex

import java.{util => ju, lang => jl}
import scala.runtime.AbstractFunction1


// just to look nicer in Java
trait Expression[-C <: ExpressionContext, +R] extends (C => R)

abstract class AbstractExpression[-C <: ExpressionContext, +R] extends AbstractFunction1[C, R] with Expression[C, R]
