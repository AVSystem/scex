package com.avsystem.scex.validation

import SymbolValidator._
import com.avsystem.scex.util.CommonUtils._
import java.{util => ju, lang => jl}
import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.reflect.api.{Universe, TypeCreator}
import scala.reflect.macros.Context
import com.avsystem.scex.util.MacroUtils

class SymbolValidator(accessSpecs: List[MemberAccessSpec]) {

  def isInvocationAllowed(c: Context)
    (objType: c.universe.Type, invocationSymbol: c.universe.Symbol, invocationConvOpt: Option[c.universe.Symbol]): Boolean = {

    import c.universe._
    val macroUtils = MacroUtils(c)
    import macroUtils._

    val invocationSymbolSignatures =
      (invocationSymbol :: invocationSymbol.allOverriddenSymbols).view.map(memberSignature).toSet

    def symbolsMatch(specSymbol: String, actualSymbol: Symbol) =
      invocationSymbolSignatures.contains(specSymbol)

    val invocationConvSignatureOpt = invocationConvOpt.map(memberSignature)

    accessSpecs.collectFirst {
      case MemberAccessSpec(typeCreator, specSymbol, specConvOpt, allow)
        if specConvOpt == invocationConvSignatureOpt &&
          symbolsMatch(specSymbol, invocationSymbol) &&
          objType <:< typeCreator.typeIn(c.universe) =>
        allow
    } getOrElse false
  }

  lazy val referencedJavaClasses = accessSpecs.collect({
    case MemberAccessSpec(typeInfo, _, _, true)
      if typeInfo.clazz.isDefined && typeInfo.isJava =>
      hierarchy(typeInfo.clazz.get)
  }).flatten.toSet
}

object SymbolValidator {
  private def elidedByMacro =
    throw new NotImplementedError("You cannot use this outside of symbol validator DSL")

  case class MemberAccessSpec(typeInfo: TypeInfo, member: String, implicitConv: Option[String], allow: Boolean)

  implicit class WildcardMemberAccess(wrapped: Any) {
    /**
     * Allows for calling any method on given type
     */
    def anyMethod = elidedByMacro

    /**
     * Allows for calling any overloaded variant of method with given name
     */
    def anyMethodNamed(name: String) = elidedByMacro

    /**
     * Allows for creating new instances of given type using any public constructor.
     */
    def anyConstructor = elidedByMacro

    /**
     * Allows for creating new instances of given type using constructor with given signature.
     */
    def constructorWithSignature(signature: String) = elidedByMacro

    /**
     * Allows for calling any Scala val or var getters
     */
    def anyScalaGetter = elidedByMacro

    /**
     * Allows for calling any Scala var setters
     */
    def anyScalaSetter = elidedByMacro

    /**
     * Allows for calling any Java bean getters
     */
    def anyBeanGetter = elidedByMacro

    /**
     * Allows for calling any Java bean setters
     */
    def anyBeanSetter = elidedByMacro
  }

  def allow(expr: Any): List[SymbolValidator.MemberAccessSpec] = macro SymbolValidatorMacros.allow_impl

  def deny(expr: Any): List[SymbolValidator.MemberAccessSpec] = macro SymbolValidatorMacros.deny_impl

  def on[T](expr: T => Any): T => Any = macro SymbolValidatorMacros.on_impl[T]
}
