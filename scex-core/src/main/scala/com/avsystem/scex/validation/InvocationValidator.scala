package com.avsystem.scex.validation

import java.{util => ju, lang => jl}
import scala.reflect.runtime.{universe => ru}
import scala.reflect.api.Universe

trait InvocationValidator {
  def isInvocationAllowed(u: Universe)(objType: u.Type, symbol: u.Symbol): Option[Boolean]
}

case class ClassMembersValidator(classesAndMethods: List[(Class[_], String)], allow: Boolean) extends InvocationValidator {
  def isInvocationAllowed(u: Universe)(objType: u.Type, symbol: u.Symbol): Option[Boolean] = ???
}

case class AlwaysMatchingValidator(allow: Boolean) extends InvocationValidator {
  def isInvocationAllowed(u: Universe)(objType: u.Type, symbol: u.Symbol): Option[Boolean] =
    Some(allow)
}
