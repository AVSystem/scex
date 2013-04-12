package com.avsystem.scex

import java.{util => ju, lang => jl}
import scala.reflect.runtime.{universe => ru}

trait Expression[-T, +R] extends (T => R)
