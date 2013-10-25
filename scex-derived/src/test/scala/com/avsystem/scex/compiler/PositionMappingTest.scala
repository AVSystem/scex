package com.avsystem.scex.compiler

import java.{util => ju, lang => jl}
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import scala.collection.immutable.SortedMap
import com.avsystem.scex.compiler.xmlfriendly.{ShiftInfo, PositionMapping}

/**
 * Created: 24-10-2013
 * Author: ghik
 */
@RunWith(classOf[JUnitRunner])
class PositionMappingTest extends FunSuite {
  test("empty mapping test") {
    val mapping = new PositionMapping(SortedMap.empty, SortedMap.empty)
    val reverse = mapping.reverse

    for (i <- -5 to 5) {
      assert(mapping(i) === i)
    }
    for (i <- -5 to 5) {
      assert(reverse(i) === i)
    }
  }

  test("something was added at the beginning") {
    val added = 5
    val mapping = new PositionMapping(SortedMap(
      0 -> ShiftInfo(0, added)
    ), null)

    for (i <- -5 until 0) {
      assert(mapping(i) === i)
    }
    for (i <- 0 to 10) {
      assert(mapping(i) === i + added)
    }
  }

  test("something was removed at the beginning") {
    val removed = 5
    val mapping = new PositionMapping(SortedMap(
      0 -> ShiftInfo(0, -removed)
    ), null)

    for (i <- -5 until 0) {
      assert(mapping(i) === i)
    }
    for (i <- 0 to removed) {
      assert(mapping(i) === 0)
    }
    for (i <- removed to 10) {
      assert(mapping(i) == i - removed)
    }
  }

  test("something was added and removed at the beginning") {
    val added = 3
    val removed = 5
    val mapping = new PositionMapping(SortedMap(
      0 -> ShiftInfo(0, added, removed)
    ), null)

    for (i <- -5 until 0) {
      assert(mapping(i) === i)
    }
    for (i <- 0 until removed) {
      assert(mapping(i) === 0)
    }
    for (i <- removed to 10) {
      assert(mapping(i) == i - removed + added)
    }
  }

  test("more complex test") {
    /*
    0123     45678 901234567
    oooraaaaaoorrraorroooooo
    012334567890000122234567
    */

    val mapping = new PositionMapping(SortedMap(
      3 -> ShiftInfo(0, 5, 1),
      6 -> ShiftInfo(4, 1, 3),
      10 -> ShiftInfo(2, 0, 2)
    ), null)

    val results = Array(0, 1, 2, 3, 8, 9, 10, 10, 10, 11, 12, 12, 12, 13, 14, 15, 16, 17, 18)
    for (i <- -5 until 0) {
      assert(mapping(i) === i)
    }
    for (i <- 0 until results.length) {
      assert(mapping(i) === results(i))
    }
  }
}
