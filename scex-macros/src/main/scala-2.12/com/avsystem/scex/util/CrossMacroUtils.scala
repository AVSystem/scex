package com.avsystem.scex.util

trait CrossMacroUtils { this: MacroUtils =>

  import universe._

  type CrossNamedArg = AssignOrNamedArg
  object CrossNamedArg {
    def apply(lhs: Tree, rhs: Tree): CrossNamedArg = AssignOrNamedArg(lhs, rhs)
    def unapply(tree: CrossNamedArg): Option[(Tree, Tree)] = AssignOrNamedArg.unapply(tree)
  }
}
