package com.avsystem.scex

import java.{util => ju, lang => jl}
import scala.runtime.AbstractFunction1


// just to look nicer in Java
trait Expression[-C <: ExpressionContext[_, _], +R] extends (C => R)

abstract class AbstractExpression[-C <: ExpressionContext[_, _], +R] extends AbstractFunction1[C, R] with Expression[C, R]
