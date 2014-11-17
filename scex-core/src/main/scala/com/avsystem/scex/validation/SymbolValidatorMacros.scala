package com.avsystem.scex
package validation

import java.{lang => jl, util => ju}

import com.avsystem.scex.symboldsl.SymbolInfoParser
import com.avsystem.scex.validation.SymbolValidator.MemberAccessSpec

import scala.reflect.macros.whitebox

object SymbolValidatorMacros {
  def allow_impl(c: whitebox.Context)(expr: c.Expr[Any]): c.Expr[List[MemberAccessSpec]] =
    extractMemberAccessSpecs(c)(expr, allow = true)

  def deny_impl(c: whitebox.Context)(expr: c.Expr[Any]): c.Expr[List[MemberAccessSpec]] =
    extractMemberAccessSpecs(c)(expr, allow = false)

  private def extractMemberAccessSpecs(c: whitebox.Context)(expr: c.Expr[Any], allow: Boolean): c.Expr[List[MemberAccessSpec]] = {
    SymbolInfoParser(SymbolValidator, c)(c.literal(allow)).extractSymbolInfos(expr.tree)
  }
}
