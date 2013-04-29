package com.avsystem.scex.compiler

import java.{util => ju, lang => jl}
import scala.reflect.runtime.{universe => ru}

// just to look nicer in Java
trait Expression[-T, +R] extends (T => R)
