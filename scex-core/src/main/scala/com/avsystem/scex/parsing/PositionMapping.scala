package com.avsystem.scex.parsing

import com.github.ghik.silencer.silent

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

  def andThen(other: PositionMapping): PositionMapping =
    other compose this
}

case class ShiftInfo(totalPrevShift: Int, addedLeft: Int, removedLeft: Int, addedRight: Int, removedRight: Int) {
  def update(amount: Int, binding: Binding): ShiftInfo =
    if (amount > 0 && binding == Binding.Left)
      copy(addedLeft = addedLeft + amount)
    else if (amount < 0 && binding == Binding.Left)
      copy(removedLeft = removedLeft - amount)
    else if (amount > 0 && binding == Binding.Right)
      copy(addedRight = addedRight + amount)
    else if (amount < 0 && binding == Binding.Right)
      copy(removedRight = removedRight - amount)
    else this

  def totalShift: Int =
    totalPrevShift + addedLeft - removedLeft + addedRight - removedRight
}

object ShiftInfo {
  def empty(totalPrevShift: Int): ShiftInfo =
    new ShiftInfo(totalPrevShift, 0, 0, 0, 0)

  def apply(totalPrevShift: Int, amount: Int, binding: Binding): ShiftInfo =
    empty(totalPrevShift).update(amount, binding)

  def apply(totalPrevShift: Int, added: Int, removed: Int, binding: Binding): ShiftInfo =
    empty(totalPrevShift).update(added, binding).update(-removed, binding)
}

class ShiftInfoPositionMapping(
  private val shiftMapping: SortedMap[Int, ShiftInfo],
  private val reverseShiftMapping: SortedMap[Int, ShiftInfo]
) extends PositionMapping {

  @silent("deprecated")
  def apply(pos: Int): Int = shiftMapping.to(pos).lastOption match {
    case Some((offset, si)) =>
      // removedleft|removedright
      //   addedleft|addedright
      //
      // All 'removedleft' positions map to the first position of 'addedleft' or last position before it if empty.
      // All 'removedright' positions map to the first position of 'addedright' or first position after it if empty.
      val relpos = pos - offset
      val reloffset = offset + si.totalPrevShift
      if (relpos < si.removedLeft)
        reloffset - (if (si.addedLeft == 0 && reloffset > 0) 1 else 0)
      else if (relpos < si.removedLeft + si.removedRight)
        reloffset + si.addedLeft
      else
        pos + si.totalShift

    case None =>
      pos
  }

  def reverse: PositionMapping =
    new ShiftInfoPositionMapping(reverseShiftMapping, shiftMapping)

  override def equals(other: Any): Boolean = other match {
    case op: ShiftInfoPositionMapping => shiftMapping == op.shiftMapping
    case _ => false
  }

  override lazy val hashCode: Int =
    shiftMapping.hashCode()

  override def toString: String =
    s"PositionMapping($shiftMapping)"
}

case class SingleShiftPositionMapping(amount: Int) extends PositionMapping {
  def apply(pos: Int): Int = pos + amount

  def reverse: PositionMapping = SingleShiftPositionMapping(-amount)
}

case class ComposedPositionMapping(left: PositionMapping, right: PositionMapping) extends PositionMapping {
  def apply(pos: Int): Int = left(right(pos))

  def reverse: PositionMapping = ComposedPositionMapping(right.reverse, left.reverse)
}

object EmptyPositionMapping extends PositionMapping {
  def apply(pos: Int): Int = pos

  def reverse: PositionMapping = this
}
