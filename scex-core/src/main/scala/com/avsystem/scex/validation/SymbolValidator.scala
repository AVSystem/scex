package com.avsystem.scex.validation

import SymbolValidator._
import com.avsystem.scex.util.CommonUtils._
import com.avsystem.scex.util.MacroUtils
import java.{util => ju, lang => jl}
import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.language.dynamics
import scala.reflect.macros.Context

class SymbolValidator(val accessSpecs: List[MemberAccessSpec]) {

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

  import SymbolValidatorMacros._

  private def elidedByMacro =
    throw new NotImplementedError("You cannot use this outside of symbol validator DSL")

  case class MemberAccessSpec(typeInfo: TypeInfo, member: String, implicitConv: Option[String], allow: Boolean) {
    override def toString = {
      (if (allow) "Allowed" else "Denied") +
        s" on type $typeInfo method $member" +
        implicitConv.map(ic => s" by implicit conversion $ic").getOrElse("")
    }
  }

  implicit def toDirectWildcardSelector(any: Any): DirectWildcardSelector = elidedByMacro

  // indicates end of wildcard selector
  trait CompleteWildcardSelector

  trait WildcardSelector {
    def all: ScopeSpecifiers with MethodSubsets
  }

  trait DirectWildcardSelector extends WildcardSelector {
    def all: ScopeSpecifiers with DirectMethodSubsets

    def constructorWithSignature(signature: String): CompleteWildcardSelector

    def implicitlyAs[T]: WildcardSelector
  }

  trait ScopeSpecifiers {
    def declared: ScalaMethodSubsets

    def introduced: ScalaMethodSubsets
  }

  trait MethodSubsets {
    def methods: CompleteWildcardSelector

    def methodsNamed: MethodsNamed

    def methodsNamed(name: String): CompleteWildcardSelector = macro methodsNamed_impl

    def beanGetters: CompleteWildcardSelector

    def beanSetters: CompleteWildcardSelector
  }

  trait ScalaMethodSubsets extends MethodSubsets {
    def scalaGetters: CompleteWildcardSelector

    def scalaSetters: CompleteWildcardSelector
  }

  trait DirectMethodSubsets extends ScalaMethodSubsets {
    def constructors: CompleteWildcardSelector
  }

  trait MethodsNamed extends Dynamic {
    def selectDynamic(name: String): CompleteWildcardSelector
  }

  def allow(expr: Any): List[SymbolValidator.MemberAccessSpec] = macro allow_impl

  def deny(expr: Any): List[SymbolValidator.MemberAccessSpec] = macro deny_impl

  def on[T](expr: T => Any): T => Any = macro on_impl[T]

  def allStatic[T]: MethodSubsets = elidedByMacro
}
