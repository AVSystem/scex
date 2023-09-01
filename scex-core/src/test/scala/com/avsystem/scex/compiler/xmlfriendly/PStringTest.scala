package com.avsystem.scex
package compiler.xmlfriendly

import com.avsystem.scex.parsing.{Binding, Modification, PString, ShiftInfo}
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.SortedMap

/**
  * Created: 25-10-2013
  * Author: ghik
  */
class PStringTest extends AnyFunSuite {
  def test(name: String)(modifications: Modification*)
    (shiftMapping: (Int, ShiftInfo)*)(reverseShiftMapping: (Int, ShiftInfo)*): Unit = super.test(name) {
    val (actualShiftMapping, actualReverseShiftMapping) =
      PString.computeMapping(modifications.toList, Nil, Nil)

    assert(actualShiftMapping == SortedMap(shiftMapping: _*), "actual mapping")
    assert(actualReverseShiftMapping == SortedMap(reverseShiftMapping: _*), "reverse mapping")
  }

  test("no modifications test")()()()

  test("single add test")(
    Modification(0, 5, Binding.Left))(
    0 -> ShiftInfo(0, 5, 0, Binding.Left))(
    0 -> ShiftInfo(0, 0, 5, Binding.Left))

  test("single remove test")(
    Modification(0, -5, Binding.Left))(
    0 -> ShiftInfo(0, 0, 5, Binding.Left))(
    0 -> ShiftInfo(0, 5, 0, Binding.Left))

  test("remove+add replace test")(
    Modification(0, -5, Binding.Left), Modification(0, 3, Binding.Left))(
    0 -> ShiftInfo(0, 3, 5, Binding.Left))(
    0 -> ShiftInfo(0, 5, 3, Binding.Left))

  test("add+remove replace test")(
    Modification(0, 3, Binding.Left), Modification(0, -5, Binding.Left))(
    0 -> ShiftInfo(0, 3, 5, Binding.Left))(
    0 -> ShiftInfo(0, 5, 3, Binding.Left))

  test("consecutive remove and add test")(
    Modification(0, -5, Binding.Left), Modification(5, 3, Binding.Left))(
    0 -> ShiftInfo(0, 0, 5, Binding.Left), 5 -> ShiftInfo(-5, 3, 0, Binding.Left))(
    0 -> ShiftInfo(0, 5, 3, Binding.Left))

  test("complex test")(
    Modification(2, 5, Binding.Left),
    Modification(3, -4, Binding.Left),
    Modification(10, -2, Binding.Left),
    Modification(12, 4, Binding.Left)
  )(
    2 -> ShiftInfo(0, 5, 0, Binding.Left),
    3 -> ShiftInfo(5, 0, 4, Binding.Left),
    10 -> ShiftInfo(1, 0, 2, Binding.Left),
    12 -> ShiftInfo(-1, 4, 0, Binding.Left)
  )(
    2 -> ShiftInfo(0, 0, 5, Binding.Left),
    8 -> ShiftInfo(-5, 4, 0, Binding.Left),
    11 -> ShiftInfo(-1, 2, 4, Binding.Left)
  )
}
