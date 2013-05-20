package com.avsystem.scex.validation

import SymbolValidator._
import com.avsystem.scex.util.CommonUtils._
import com.avsystem.scex.util.MacroUtils
import java.{util => ju, lang => jl}
import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.reflect.macros.Context

class SymbolValidator(accessSpecs: List[MemberAccessSpec]) {

  lazy val referencedJavaClasses = accessSpecs.collect({
    case MemberAccessSpec(typeInfo, _, _, true)
      if typeInfo.clazz.isDefined && typeInfo.isJava =>
      hierarchy(typeInfo.clazz.get)
  }).flatten.toSet

  private type SignatureWithConversion = (String, Option[String])
  // MemberAccessSpec with its index in ACL (accessSpecs)
  private type SpecWithIndex = (MemberAccessSpec, Int)

  private val bySignaturesMap: Map[SignatureWithConversion, List[SpecWithIndex]] =
    accessSpecs.zipWithIndex.groupBy {
      case (MemberAccessSpec(_, methodSignature, implicitConvSignatureOpt, _), _) =>
        (methodSignature, implicitConvSignatureOpt)
    }.toMap.withDefaultValue(Nil)

  def isInvocationAllowed(c: Context)
    (objType: c.universe.Type, invokedSymbol: c.universe.Symbol, invokedConvOpt: Option[c.universe.Symbol]): Boolean = {

    val macroUtils = MacroUtils(c)
    import macroUtils._

    val invocationConvSignatureOpt: Option[String] = invokedConvOpt.map(memberSignature)

    // signatures of invoked symbol and all symbols overridden by it
    val allSignatures: List[SignatureWithConversion] =
      (invokedSymbol :: invokedSymbol.allOverriddenSymbols).map {
        symbol => (memberSignature(symbol), invocationConvSignatureOpt)
      }.toSet.toList

    // MemberAccessSpecs that match this invocation
    val matchingSpecs: List[SpecWithIndex] = allSignatures.flatMap { signature =>
      bySignaturesMap(signature).filter {
        case (accessSpec, _) => objType <:< accessSpec.typeInfo.typeIn(c.universe)
      }
    }

    // get 'allow' field from matching spec that appeared first in ACL or false if there was no matching spec
    if (matchingSpecs.nonEmpty) matchingSpecs.minBy(_._2)._1.allow else false
  }

}

object SymbolValidator {
  private def elidedByMacro =
    throw new NotImplementedError("You cannot use this outside of symbol validator DSL")

  case class MemberAccessSpec(typeInfo: TypeInfo, member: String, implicitConv: Option[String], allow: Boolean) {
    override def toString = {
      (if (allow) "Allowed" else "Denied") +
        s" method $member on type $typeInfo" +
        implicitConv.map(ic => s" by implicit conversion $ic").getOrElse("")
    }
  }

  implicit class WildcardMemberAccess(wrapped: Any) {
    /**
     * Allows for calling any method on given type, excluding constructors.
     */
    def anyMethod = elidedByMacro

    /**
     * Allows for calling any method on given type declared in class that defines this type, excluding constructors.
     */
    def anyDeclaredMethod = elidedByMacro

    /**
     * Allows for calling any overloaded variant of method with given name.
     */
    def anyMethodNamed(name: String) = elidedByMacro

    /**
     * Allows for creating new instances of given type using any public constructor.
     */
    def anyConstructor = elidedByMacro

    /**
     * Allows for creating new instances of given type using constructor with given signature.
     * Used mainly when given constructor cannot be expressed as plain invocation in SymbolValidator DSL.
     */
    def constructorWithSignature(signature: String) = elidedByMacro

    /**
     * Allows for calling any Scala val or var getters.
     */
    def anyScalaGetter = elidedByMacro

    /**
     * Allows for calling any Scala var setters.
     */
    def anyScalaSetter = elidedByMacro

    /**
     * Allows for calling any Java bean getters.
     */
    def anyBeanGetter = elidedByMacro

    /**
     * Allows for calling any Java bean setters.
     */
    def anyBeanSetter = elidedByMacro
  }

  def allow(expr: Any): List[SymbolValidator.MemberAccessSpec] = macro SymbolValidatorMacros.allow_impl

  def deny(expr: Any): List[SymbolValidator.MemberAccessSpec] = macro SymbolValidatorMacros.deny_impl

  def on[T](expr: T => Any): T => Any = macro SymbolValidatorMacros.on_impl[T]
}
