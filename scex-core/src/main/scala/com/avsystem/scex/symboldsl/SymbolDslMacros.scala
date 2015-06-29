package com.avsystem.scex.symboldsl

import java.{lang => jl, util => ju}

import com.avsystem.scex.presentation.{Attributes, SymbolAttributes}
import com.avsystem.scex.validation.SymbolValidator
import com.avsystem.scex.validation.SymbolValidator.MemberAccessSpec

import scala.reflect.macros.whitebox

class SymbolDslMacros(val c: whitebox.Context) {
  import c.universe._

  def allow_impl(expr: c.Expr[Any]): c.Expr[List[MemberAccessSpec]] =
    extractMemberAccessSpecs(expr, allow = true)

  def deny_impl(expr: c.Expr[Any]): c.Expr[List[MemberAccessSpec]] =
    extractMemberAccessSpecs(expr, allow = false)

  private def extractMemberAccessSpecs(expr: c.Expr[Any], allow: Boolean): c.Expr[List[MemberAccessSpec]] = {
    SymbolInfoParser(SymbolValidator, c)(c.literal(allow)).extractSymbolInfos(expr.tree)
  }

  def attributes_impl(any: c.Expr[Any]): c.Expr[List[SymbolInfo[Attributes]]] = {
    SymbolInfoParser(SymbolAttributes, c)(reify(Attributes.empty)).extractSymbolInfos(any.tree)
  }
}
