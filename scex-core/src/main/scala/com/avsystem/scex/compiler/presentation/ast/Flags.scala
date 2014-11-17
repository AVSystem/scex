package com.avsystem.scex
package compiler.presentation.ast

import java.{lang => jl, util => ju}

import scala.reflect.internal.ModifierFlags

/**
 * Created: 12-03-2014
 * Author: ghik
 */
case class Flags(flags: Long)(flagString: String) {
  private def hasFlag(flag: Long) = (flags & flag) != 0

  override def toString =
    flagString

  def isDefaultInit = hasFlag(ModifierFlags.DEFAULTINIT)

  def isPreSuper = hasFlag(ModifierFlags.PRESUPER)

  def isDefaultParam = hasFlag(ModifierFlags.DEFAULTPARAM)

  def isContravariant = hasFlag(ModifierFlags.CONTRAVARIANT)

  def isCovariant = hasFlag(ModifierFlags.COVARIANT)

  def isParam = hasFlag(ModifierFlags.PARAM)

  def isByNameParam = hasFlag(ModifierFlags.BYNAMEPARAM)

  def isAbstractOverride = hasFlag(ModifierFlags.ABSOVERRIDE)

  def isCase = hasFlag(ModifierFlags.CASE)

  def isLocal = hasFlag(ModifierFlags.LOCAL)

  def isProtected = hasFlag(ModifierFlags.PROTECTED)

  def isPrivate = hasFlag(ModifierFlags.PRIVATE)

  def isOverride = hasFlag(ModifierFlags.OVERRIDE)

  def isLazy = hasFlag(ModifierFlags.LAZY)

  def isImplicit = hasFlag(ModifierFlags.IMPLICIT)

  def isSealed = hasFlag(ModifierFlags.SEALED)

  def isFinal = hasFlag(ModifierFlags.FINAL)

  def isAbstract = hasFlag(ModifierFlags.ABSTRACT)

  def isDeferred = hasFlag(ModifierFlags.DEFERRED)

  def isMacro = hasFlag(ModifierFlags.MACRO)

  def isMutable = hasFlag(ModifierFlags.MUTABLE)

  def isInterface = hasFlag(ModifierFlags.INTERFACE)

  def isTrait = hasFlag(ModifierFlags.TRAIT)
}

object Flags {
  final val empty = Flags(0)("")
}