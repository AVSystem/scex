package com.avsystem.scex.parsing

import java.{lang => jl, util => ju}

import scala.collection.immutable.SortedMap

/**
 * Created: 24-10-2013
 * Author: ghik
 */
trait PositionMapping {
  def apply(pos: Int): Int

  def reverse: PositionMapping

  def compose(other: PositionMapping): PositionMapping =
    if (this eq EmptyPositionMapping) other
    else if (other eq EmptyPositionMapping) this
    else ComposedPositionMapping(this, other)

  def andThen(other: PositionMapping) =
    other compose this
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

class ShiftInfoPositionMapping(
  private val shiftMapping: SortedMap[Int, ShiftInfo],
  private val reverseShiftMapping: SortedMap[Int, ShiftInfo]) extends PositionMapping {

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
    new ShiftInfoPositionMapping(reverseShiftMapping, shiftMapping)

  override def equals(other: Any) = other match {
    case op: ShiftInfoPositionMapping => shiftMapping == op.shiftMapping
    case _ => false
  }

  override lazy val hashCode =
    shiftMapping.hashCode()

  override def toString =
    s"PositionMapping($shiftMapping)"
}

case class SingleShiftPositionMapping(amount: Int) extends PositionMapping {
  def apply(pos: Int) = pos + amount

  def reverse = SingleShiftPositionMapping(-amount)
}

case class ComposedPositionMapping(left: PositionMapping, right: PositionMapping) extends PositionMapping {
  def apply(pos: Int) = left(right(pos))

  def reverse = ComposedPositionMapping(right.reverse, left.reverse)
}

object EmptyPositionMapping extends PositionMapping {
  def apply(pos: Int) = pos

  def reverse = this
}
