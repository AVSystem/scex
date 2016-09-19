package com.avsystem.scex.symboldsl

import java.{lang => jl, util => ju}

import com.avsystem.commons.macros.AbstractMacroCommons
import com.avsystem.scex.util.MacroUtils

import scala.reflect.macros.blackbox

class SymbolDslMacros(ctx: blackbox.Context) extends AbstractMacroCommons(ctx) with MacroUtils { self =>

  import c.universe._

  lazy val universe: c.universe.type = c.universe

  lazy val AttributesObj = q"$ScexPkg.presentation.Attributes"
  lazy val SymbolValidatorObj = q"$ScexPkg.validation.SymbolValidator"
  lazy val SymbolAttributesObj = q"$ScexPkg.presentation.SymbolAttributes"

  def allow_impl(expr: c.Expr[Any]): c.Tree =
    extractMemberAccessSpecs(expr, allow = true)

  def deny_impl(expr: c.Expr[Any]): c.Tree =
    extractMemberAccessSpecs(expr, allow = false)

  private def extractMemberAccessSpecs(expr: c.Expr[Any], allow: Boolean): c.Tree =
    new SymbolInfoParser[self.c.type](self.c) {
      def defaultPayload = q"$allow"
      def dslObject = SymbolValidatorObj
    }.extractSymbolInfos(expr.tree)

  def attributes_impl(any: c.Expr[Any]): c.Tree =
    new SymbolInfoParser[self.c.type](self.c) {
      def defaultPayload = q"$AttributesObj.empty"
      def dslObject = SymbolAttributesObj
    }.extractSymbolInfos(any.tree)
}
