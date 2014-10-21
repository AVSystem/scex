package com.avsystem.scex
package compiler.xmlfriendly

import java.{util => ju, lang => jl}
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import scala.collection.immutable.SortedMap
import com.avsystem.scex.compiler.{ScexFunSuite, ShiftInfo}

/**
 * Created: 25-10-2013
 * Author: ghik
 */
class PStringTest extends ScexFunSuite {
  def test(name: String)(modifications: Modification*)
    (shiftMapping: (Int, ShiftInfo)*)(reverseShiftMapping: (Int, ShiftInfo)*): Unit = super.test(name) {
    val (actualShiftMapping, actualReverseShiftMapping) =
      PString.computeMapping(modifications.toList, Nil, Nil)

    assert(actualShiftMapping === SortedMap(shiftMapping: _*), "actual mapping")
    assert(actualReverseShiftMapping === SortedMap(reverseShiftMapping: _*), "reverse mapping")
  }

  test("no modifications test")()()()

  test("single add test")(
    Modification(0, 5))(
      0 -> ShiftInfo(0, 5, 0))(
      0 -> ShiftInfo(0, 0, 5))

  test("single remove test")(
    Modification(0, -5))(
      0 -> ShiftInfo(0, 0, 5))(
      0 -> ShiftInfo(0, 5, 0))

  test("remove+add replace test")(
    Modification(0, -5), Modification(0, 3))(
      0 -> ShiftInfo(0, 3, 5))(
      0 -> ShiftInfo(0, 5, 3))

  test("add+remove replace test")(
    Modification(0, 3), Modification(0, -5))(
      0 -> ShiftInfo(0, 3, 5))(
      0 -> ShiftInfo(0, 5, 3))

  test("consecutive remove and add test")(
    Modification(0, -5), Modification(5, 3))(
      0 -> ShiftInfo(0, 0, 5), 5 -> ShiftInfo(-5, 3, 0))(
      0 -> ShiftInfo(0, 5, 3))

  test("complex test")(
    Modification(2, 5), Modification(3, -4), Modification(10, -2), Modification(12, 4))(
      2 -> ShiftInfo(0, 5, 0), 3 -> ShiftInfo(5, 0, 4), 10 -> ShiftInfo(1, 0, 2), 12 -> ShiftInfo(-1, 4, 0))(
      2 -> ShiftInfo(0, 0, 5), 8 -> ShiftInfo(-5, 4, 0), 11 -> ShiftInfo(-1, 2, 4))
}
