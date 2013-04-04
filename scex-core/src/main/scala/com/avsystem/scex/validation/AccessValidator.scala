package com.avsystem.scex.validation

import java.{util => ju, lang => jl}
import scala.reflect.runtime.{universe => ru}
import scala.reflect.api.{JavaUniverse, Universe}
import com.google.common.cache.{CacheLoader, CacheBuilder}
import com.avsystem.scex.utils.MacroUtils._

case class MemberAccessSpec(prefixTpe: ru.Type, member: String, implicitConv: String)

trait AccessValidator {
  def isInvocationAllowed(u: Universe)(objType: u.Type, symbol: u.Symbol, implicitConv: u.Symbol): Option[Boolean]
}

case class TypeMembersValidator(typesAndMembers: List[MemberAccessSpec], allow: Boolean) extends AccessValidator {

  import TypeMembersValidator._

  def isInvocationAllowed(u: Universe)(objType: u.Type, symbol: u.Symbol, implicitConv: u.Symbol): Option[Boolean] = {
    type Importer = u.Importer {val from: ru.type}
    val importer = importersCache.get(u).asInstanceOf[Importer]

    val matches = typesAndMembers.exists {
      case MemberAccessSpec(matchingTpe, matchingSignature, matchingImplicitConvSignature) =>
        val requiredType = if (matchingTpe != null) importer.importType(matchingTpe) else null

        ((objType == null && requiredType == null) || (objType != null && requiredType != null && objType <:< requiredType)) &&
          (symbol :: symbol.allOverriddenSymbols).exists(s => memberSignature(s) == matchingSignature) &&
          memberSignature(implicitConv) == matchingImplicitConvSignature
    }

    if (matches) Some(allow) else None
  }
}

object TypeMembersValidator {
  private val importersCache = CacheBuilder.newBuilder().weakKeys().build(new CacheLoader[Universe, Universe#Importer] {
    def load(u: Universe): Universe#Importer = u.mkImporter(ru)
  })
}

case class AlwaysMatchingValidator(allow: Boolean) extends AccessValidator {
  def isInvocationAllowed(u: Universe)(objType: u.Type, symbol: u.Symbol, implicitConv: u.Symbol): Option[Boolean] =
    Some(allow)
}

case class ChainValidator(validators: List[AccessValidator]) extends AccessValidator {
  def isInvocationAllowed(u: Universe)(objType: u.Type, symbol: u.Symbol, implicitConv: u.Symbol): Option[Boolean] =
    validators.view.map(_.isInvocationAllowed(u)(objType, symbol, implicitConv)).find(_.isDefined).getOrElse(None)
}
