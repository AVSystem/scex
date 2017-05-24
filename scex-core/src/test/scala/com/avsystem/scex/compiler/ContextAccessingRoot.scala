package com.avsystem.scex.compiler

import com.avsystem.scex.ExpressionContext

class ContextAccessingRoot {
  def gimmeVar(name: String)(implicit ctx: ExpressionContext[ContextAccessingRoot, String]): String =
    ctx.getVariable(name)
}
