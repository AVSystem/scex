package com.avsystem.scex
package compiler.presentation.ast

sealed trait Name {
  val name: String

  def isTerm = false

  def isType = false
}

case class TermName(name: String) extends Name {
  override def isTerm = true
}

object TermName {
  final val WILDCARD = TermName("_")
  final val ERROR = TermName("<error>")
  final val EMPTY = TermName("<empty>")
  final val PACKAGE = TermName("<package>")
  final val CONSTRUCTOR = TermName("<init>")
  final val ROOTPKG = TermName("_root_")
}

case class TypeName(name: String) extends Name {
  override def isType = true
}

object TypeName {
  final val WILDCARD = TypeName("_")
  final val ERROR = TypeName("<error>")
  final val EMPTY = TypeName("<empty>")
  final val PACKAGE = TypeName("<package>")
  final val WILDCARD_STAR = TypeName("_*")
}
