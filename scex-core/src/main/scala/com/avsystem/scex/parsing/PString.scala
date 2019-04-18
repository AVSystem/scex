package com.avsystem.scex.parsing

import com.avsystem.commons.misc.{AbstractValueEnum, AbstractValueEnumCompanion, EnumCtx}

import scala.collection.immutable.SortedMap

final class Binding(implicit enumCtx: EnumCtx) extends AbstractValueEnum
object Binding extends AbstractValueEnumCompanion[Binding] {
  final val Left, Right: Value = new Binding
}

/**
  * Created: 24-10-2013
  * Author: ghik
  */
case class Modification(offset: Int, amount: Int, binding: Binding)

case class Bound(str: String, binding: Binding) {
  def +(pstr: PString): PString =
    PString(str, pstr.beg, pstr.beg, Vector(Modification(pstr.beg, str.length, binding))) + pstr
}

case class PString(result: String, beg: Int, end: Int, mods: Vector[Modification]) {
  lazy val positionMapping: PositionMapping = {
    val normalizedMods = if (beg > 0) Modification(0, -beg, Binding.Right) :: mods.toList else mods.toList
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
          (mods :+ Modification(end, end - otherBeg, Binding.Right)) ++ otherMods

      PString(result + otherResult, beg, otherEnd, newMods)
  }

  def +(other: Bound): PString =
    PString(result + other.str, beg, end, mods :+ Modification(end, other.str.length, other.binding))

  def +(otherOpt: Option[PString]): PString =
    otherOpt match {
      case Some(pstr) => this + pstr
      case None => this
    }

  def replaceWith(replacement: String, binding: Binding): PString =
    copy(result = replacement, mods =
      Modification(beg, -result.length, binding) +: Modification(beg, replacement.length, binding) +: mods)

  def withResult(result: String): PString =
    copy(result = result)
}

object PString {
  private[scex] def computeMapping(
    mods: List[Modification],
    acc: List[(Int, ShiftInfo)],
    racc: List[(Int, ShiftInfo)]
  ): (SortedMap[Int, ShiftInfo], SortedMap[Int, ShiftInfo]) =
    (mods, acc, racc) match {
      case (Modification(offset, amount, binding) :: tail, (prevOffset, prevInfo) :: accTail, (rprevOffset, rprevInfo) :: raccTail) =>
        val newAcc = if (offset == prevOffset)
          (prevOffset, prevInfo.update(amount, binding)) :: accTail
        else
          (offset, ShiftInfo(prevInfo.totalShift, amount, binding)) :: acc

        val roffset = offset + (if (offset == prevOffset) prevInfo.totalPrevShift else prevInfo.totalShift)
        val newRacc = if (roffset == rprevOffset)
          (rprevOffset, rprevInfo.update(-amount, binding)) :: raccTail
        else
          (roffset, ShiftInfo(rprevInfo.totalShift, -amount, binding)) :: racc

        computeMapping(tail, newAcc, newRacc)

      case (Modification(offset, amount, binding) :: tail, Nil, Nil) =>
        computeMapping(tail, List((offset, ShiftInfo(0, amount, binding))), List((offset, ShiftInfo(0, -amount, binding))))

      case (Nil, _, _) =>
        (SortedMap(acc: _*), SortedMap(racc: _*))

      case tuple =>
        throw new IllegalArgumentException(tuple.toString())
    }
}