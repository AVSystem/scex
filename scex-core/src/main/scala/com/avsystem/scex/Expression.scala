package com.avsystem.scex

import java.{util => ju, lang => jl}


// just to look nicer in Java
trait Expression[-T, +R] extends (T => R)
