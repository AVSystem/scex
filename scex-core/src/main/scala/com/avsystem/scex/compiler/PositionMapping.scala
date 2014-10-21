package com.avsystem.scex
package compiler

import java.{lang => jl, util => ju}

import scala.collection.immutable.SortedMap

/**
 * Created: 24-10-2013
 * Author: ghik
 */
class PositionMapping(
  private val shiftMapping: SortedMap[Int, ShiftInfo],
  private val reverseShiftMapping: SortedMap[Int, ShiftInfo]) {

  def apply(pos: Int) = shiftMapping.to(pos).lastOption match {
    case Some((offset, ShiftInfo(totalPrevShift, added, removed))) =>
      if (pos - offset < removed)
        offset + totalPrevShift
      else
        pos + totalPrevShift - removed + added

    case None =>
      pos
  }

  def reverse =
    new PositionMapping(reverseShiftMapping, shiftMapping)

  override def equals(other: Any) = other match {
    case op: PositionMapping => shiftMapping == op.shiftMapping
    case _ => false
  }

  override lazy val hashCode =
    shiftMapping.hashCode()

  override def toString =
    s"PositionMapping($shiftMapping)"
}

object PositionMapping {
  val empty = new PositionMapping(SortedMap.empty, SortedMap.empty)
}

case class ShiftInfo(totalPrevShift: Int, added: Int, removed: Int) {
  def this(totalPrevShift: Int, amount: Int) =
    this(totalPrevShift, math.max(0, amount), math.max(0, -amount))

  def update(amount: Int) = if (amount > 0)
    copy(added = added + amount)
  else if (amount < 0)
    copy(removed = removed - amount)
  else this

  def totalShift = totalPrevShift + added - removed
}

object ShiftInfo {
  def apply(totalPrevShift: Int, amount: Int) =
    new ShiftInfo(totalPrevShift, amount)
}