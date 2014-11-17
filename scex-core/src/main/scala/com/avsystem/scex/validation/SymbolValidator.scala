package com.avsystem.scex
package validation

import java.{lang => jl, util => ju}

import com.avsystem.scex.symboldsl.{SymbolDsl, SymbolInfo, SymbolInfoList}
import com.avsystem.scex.util.CommonUtils._
import com.avsystem.scex.util.LoggingUtils

import scala.language.experimental.macros
import scala.language.{dynamics, implicitConversions}

trait SymbolValidator extends SymbolInfoList[Boolean] with LoggingUtils {
  private val logger = createLogger[SymbolValidator]

  def combine(otherValidator: SymbolValidator) =
    SymbolValidator(infoList ++ otherValidator.infoList)

  lazy val referencedJavaClasses = infoList.collect({
    case SymbolInfo(typeInfo, _, _, true) if typeInfo.clazz.isDefined && typeInfo.isJava =>
      hierarchy(typeInfo.clazz.get)
  }).flatten.toSet

  private lazy val specsLength = infoList.length

  private def lowestPriority(allowedByDefault: Boolean) =
    if (allowedByDefault) specsLength else specsLength + 1

  def referencesModuleMember(moduleSymbolFullName: String) = {
    val prefix = moduleSymbolFullName + "."
    val projection = memberSignatures.from(prefix)
    projection.nonEmpty && projection.firstKey.startsWith(prefix)
  }

  def validateMemberAccess(vc: ValidationContext)(access: vc.MemberAccess): vc.ValidationResult = {
    import vc._

    access match {
      case access@SimpleMemberAccess(tpe, symbol, implicitConv, allowedByDefault, position) =>
        logger.trace(s"Validating access: $access")

        // SymbolInfos that match this invocation
        val matchingSpecs = matchingInfos(vc.universe)(tpe, symbol, implicitConv)

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
      val infoList = acl
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
