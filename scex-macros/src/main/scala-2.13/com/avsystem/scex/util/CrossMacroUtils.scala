package com.avsystem.scex.util

trait CrossMacroUtils { this: MacroUtils =>
  type CrossNamedArg = universe.NamedArg
  final lazy val CrossNamedArg = universe.NamedArg
}
