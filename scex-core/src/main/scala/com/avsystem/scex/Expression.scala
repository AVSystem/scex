package com.avsystem.scex

import java.{util => ju, lang => jl}
import scala.runtime.AbstractFunction1


// just to look nicer in Java
trait Expression[-T, +R] extends (T => R)

abstract class AbstractExpression[-T, +R] extends AbstractFunction1[T, R] with Expression[T, R]
