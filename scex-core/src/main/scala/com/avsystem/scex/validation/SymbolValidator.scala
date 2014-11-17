package com.avsystem.scex
package validation

import java.{lang => jl, util => ju}

import com.avsystem.scex.symboldsl.{SymbolDsl, SymbolInfo}
import com.avsystem.scex.util.CommonUtils._
import com.avsystem.scex.util.LoggingUtils
import com.avsystem.scex.validation.SymbolValidator.MemberAccessSpec

import scala.collection.immutable.{SortedSet, TreeSet}
import scala.language.experimental.macros
import scala.language.{dynamics, implicitConversions}

trait SymbolValidator extends LoggingUtils {
  private val logger = createLogger[SymbolValidator]

  val accessSpecs: List[MemberAccessSpec]

  def combine(otherValidator: SymbolValidator) =
    SymbolValidator(accessSpecs ++ otherValidator.accessSpecs)

  lazy val referencedJavaClasses = accessSpecs.collect({
    case SymbolInfo(typeInfo, _, _, true) if typeInfo.clazz.isDefined && typeInfo.isJava =>
      hierarchy(typeInfo.clazz.get)
  }).flatten.toSet

  private lazy val specsLength = accessSpecs.length

  private def lowestPriority(allowedByDefault: Boolean) =
    if (allowedByDefault) specsLength else specsLength + 1

  // SymbolInfo with its index in ACL (accessSpecs)
  private type SpecWithIndex = (MemberAccessSpec, Int)

  private lazy val bySignaturesMap: Map[String, List[SpecWithIndex]] =
    accessSpecs.zipWithIndex.groupBy(_._1.memberSignature).withDefaultValue(Nil)

  private lazy val memberSignatures: SortedSet[String] =
    bySignaturesMap.keys.to[TreeSet]

  def referencesModuleMember(moduleSymbolFullName: String) = {
    val prefix = moduleSymbolFullName + "."
    val projection = memberSignatures.from(prefix)
    projection.nonEmpty && projection.firstKey.startsWith(prefix)
  }

  def validateMemberAccess(vc: ValidationContext)(access: vc.MemberAccess): vc.ValidationResult = {
    import vc._
    import vc.universe._

    def implicitConversionsMatch(actual: Option[Tree], fromSpec: Option[String]) =
      (actual, fromSpec) match {
        case (Some(actualPathTree), Some(specPath)) =>
          path(actualPathTree) == specPath
        case (None, None) => true
        case _ => false
      }

    access match {
      case access@SimpleMemberAccess(tpe, symbol, implicitConv, allowedByDefault, position) =>
        logger.trace(s"Validating access: $access")

        val signatures: List[String] =
          (symbol :: symbol.overrides).map(memberSignature)

        // SymbolInfos that match this invocation
        val matchingSpecs: List[SpecWithIndex] = signatures.flatMap { signature =>
          bySignaturesMap(signature).filter { case (accessSpec, _) =>
            signature == accessSpec.memberSignature &&
              tpe <:< accessSpec.typeInfo.typeIn(vc.universe) &&
              implicitConversionsMatch(implicitConv, accessSpec.implicitConv)
          }
        }

        def specsRepr = matchingSpecs.map({ case (spec, idx) => s"$idx: $spec"}).mkString("\n")
        logger.trace(s"Matching signatures:\n$specsRepr")

        // get 'allow' field from matching spec that appeared first in ACL or false if there was no matching spec
        val (allow, index) = if (matchingSpecs.nonEmpty) {
          val (spec, index) = matchingSpecs.minBy(_._2)
          (spec.payload, index)
        } else (allowedByDefault, lowestPriority(allowedByDefault))

        ValidationResult(index, if (allow) Nil else List(access))

      case MultipleMemberAccesses(accesses) =>
        // all must be allowed
        val (allowed, denied) = accesses.map(a => validateMemberAccess(vc)(a)).partition(_.deniedAccesses.isEmpty)
        if (denied.nonEmpty)
          ValidationResult(denied.minBy(_.priority).priority, denied.flatMap(_.deniedAccesses))
        else if (allowed.nonEmpty)
          ValidationResult(allowed.maxBy(_.priority).priority, Nil)
        else
          ValidationResult(lowestPriority(allowedByDefault = true), Nil)

      case AlternativeMemberAccess(accesses) =>
        // take the soonest-validated alternative
        if (accesses.nonEmpty)
          accesses.map(a => validateMemberAccess(vc)(a)).minBy(_.priority)
        else
          ValidationResult(lowestPriority(allowedByDefault = true), Nil)

      case NoMemberAccess =>
        ValidationResult(lowestPriority(allowedByDefault = true), Nil)
    }
  }
}

object SymbolValidator extends SymbolDsl {
  type Payload = Boolean
  type MemberAccessSpec = SymbolInfo[Boolean]

  def apply(acl: List[MemberAccessSpec]): SymbolValidator =
    new SymbolValidator {
      val accessSpecs = acl
    }

  val empty: SymbolValidator = apply(Nil)

  /**
   * Encloses block of statements that specify methods that are allowed to be called in expressions.
   * Code inside <tt>allow</tt> block is virtualized - it's not actually compiled to bytecode.
   * Multiple allow/deny blocks joined with <tt>++</tt> operator form an ACL-like structure.
   * @param expr
   * @return
   */
  def allow(expr: Any): List[MemberAccessSpec] = macro SymbolValidatorMacros.allow_impl

  /**
   * Encloses block of statements that specify methods that are not allowed to be called in expressions.
   * Code inside <tt>deny</tt> block is virtualized - it's not actually compiled to bytecode.
   * Multiple allow/deny blocks joined with <tt>++</tt> operator form an ACL-like structure.
   * @param expr
   * @return
   */
  def deny(expr: Any): List[MemberAccessSpec] = macro SymbolValidatorMacros.deny_impl

}
