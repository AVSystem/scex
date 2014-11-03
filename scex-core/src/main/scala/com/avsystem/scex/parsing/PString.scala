package com.avsystem.scex.parsing

import java.{lang => jl, util => ju}

import scala.collection.immutable.SortedMap

/**
 * Created: 24-10-2013
 * Author: ghik
 */
case class Modification(offset: Int, amount: Int)

case class PString(result: String, beg: Int, end: Int, mods: Vector[Modification]) {
  lazy val positionMapping = {
    val normalizedMods = if (beg > 0) Modification(0, -beg) :: mods.toList else mods.toList
    val (shiftMapping, reverseShiftMapping) = PString.computeMapping(normalizedMods, Nil, Nil)
    new ShiftInfoPositionMapping(shiftMapping, reverseShiftMapping)
  }

  def +(other: PString): PString = other match {
    case PString("", _, _, Vector()) =>
      this

    case PString(otherResult, otherBeg, otherEnd, otherMods) =>
      require(end <= otherBeg)

      val newMods =
        if (end == otherBeg)
          mods ++ otherMods
        else
          (mods :+ Modification(end, end - otherBeg)) ++ otherMods

      PString(result + otherResult, beg, otherEnd, newMods)
  }

  def +(other: String): PString =
    if (other.nonEmpty)
      PString(result + other, beg, end, mods :+ Modification(end, other.length))
    else this


  def +(otherOpt: Option[PString]): PString =
    otherOpt match {
      case Some(pstr) => this + pstr
      case None => this
    }

  def withResult(newResult: String) =
    copy(result = newResult)

}

object PString {
  private[scex] def computeMapping(
    mods: List[Modification],
    acc: List[(Int, ShiftInfo)],
    racc: List[(Int, ShiftInfo)]): (SortedMap[Int, ShiftInfo], SortedMap[Int, ShiftInfo]) =

    (mods, acc, racc) match {
      case (Modification(offset, amount) :: tail, (prevOffset, prevInfo) :: accTail, (rprevOffset, rprevInfo) :: raccTail) =>
        val newAcc = if (offset == prevOffset)
          (prevOffset, prevInfo.update(amount)) :: accTail
        else
          (offset, ShiftInfo(prevInfo.totalShift, amount)) :: acc

        val roffset = offset + (if (offset == prevOffset) prevInfo.totalPrevShift else prevInfo.totalShift)
        val newRacc = if (roffset == rprevOffset)
          (rprevOffset, rprevInfo.update(-amount)) :: raccTail
        else
          (roffset, ShiftInfo(rprevInfo.totalShift, -amount)) :: racc

        computeMapping(tail, newAcc, newRacc)

      case (Modification(offset, amount) :: tail, Nil, Nil) =>
        computeMapping(tail, List((offset, ShiftInfo(0, amount))), List((offset, ShiftInfo(0, -amount))))

      case (Nil, _, _) =>
        (SortedMap(acc: _*), SortedMap(racc: _*))

      case tuple =>
        throw new IllegalArgumentException(tuple.toString())
    }
}