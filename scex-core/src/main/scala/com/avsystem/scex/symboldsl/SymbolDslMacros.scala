package com.avsystem.scex.symboldsl

import java.{lang => jl, util => ju}

import com.avsystem.scex.presentation.{Attributes, SymbolAttributes}
import com.avsystem.scex.validation.SymbolValidator
import com.avsystem.scex.validation.SymbolValidator.MemberAccessSpec

import scala.reflect.macros.whitebox

object SymbolDslMacros {
  def allow_impl(c: whitebox.Context)(expr: c.Expr[Any]): c.Expr[List[MemberAccessSpec]] =
    extractMemberAccessSpecs(c)(expr, allow = true)

  def deny_impl(c: whitebox.Context)(expr: c.Expr[Any]): c.Expr[List[MemberAccessSpec]] =
    extractMemberAccessSpecs(c)(expr, allow = false)

  private def extractMemberAccessSpecs(c: whitebox.Context)(expr: c.Expr[Any], allow: Boolean): c.Expr[List[MemberAccessSpec]] = {
    SymbolInfoParser(SymbolValidator, c)(c.literal(allow)).extractSymbolInfos(expr.tree)
  }

  def attributes_impl(c: whitebox.Context)(any: c.Expr[Any]): c.Expr[List[SymbolInfo[Attributes]]] = {
    import c.universe._
    SymbolInfoParser(SymbolAttributes, c)(reify(Attributes.empty)).extractSymbolInfos(any.tree)
  }
}
