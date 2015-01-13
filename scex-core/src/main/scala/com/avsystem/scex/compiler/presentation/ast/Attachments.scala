package com.avsystem.scex
package compiler.presentation.ast

import java.{lang => jl, util => ju}

import scala.collection.JavaConverters._

/**
 * Created: 12-03-2014
 * Author: ghik
 */
case class Attachments(tpe: Type, position: Position)

object Attachments {
  val empty = new Attachments(Type.NoType, null)
}

case class Symbol(names: List[Name]) {
  def name = names.head.name

  def isTerm = names.head.isTerm

  def isType = names.head.isType

  lazy val fullName = names.reverseIterator.map(_.name).mkString(".")

  def namesAsJava = names.asJava
}

case class Position(start: Int, end: Int, transparent: Boolean) {
  def includes(other: Position) =
    other != null && start <= other.start && end >= other.end && end > other.start

  override def toString = {
    val repr = if (start != end) s"$start:$end" else start.toString
    if (transparent) s"<$repr>" else s"[$repr]"
  }
}
