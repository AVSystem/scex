package com.avsystem.scex.symboldsl

import java.{lang => jl, util => ju}

import com.avsystem.scex.presentation.{Attributes, SymbolAttributes}
import com.avsystem.scex.validation.SymbolValidator

import scala.reflect.macros.blackbox

class SymbolDslMacros(val c: blackbox.Context) {

  import c.universe._

  val AttributesObj = typeOf[Attributes.type].termSymbol

  def allow_impl(expr: c.Expr[Any]): c.Tree =
    extractMemberAccessSpecs(expr, allow = true)

  def deny_impl(expr: c.Expr[Any]): c.Tree =
    extractMemberAccessSpecs(expr, allow = false)

  private def extractMemberAccessSpecs(expr: c.Expr[Any], allow: Boolean): c.Tree =
    SymbolInfoParser(SymbolValidator, c)(q"$allow").extractSymbolInfos(expr.tree)

  def attributes_impl(any: c.Expr[Any]): c.Tree =
    SymbolInfoParser(SymbolAttributes, c)(q"$AttributesObj.empty").extractSymbolInfos(any.tree)
}
