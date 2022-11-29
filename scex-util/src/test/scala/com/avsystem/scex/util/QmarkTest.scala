package com.avsystem.scex.util

import com.avsystem.commons.JDate
import com.avsystem.scex.util.CommonExpressionUtils._
import com.avsystem.scex.util.JavaCollectionExtensions._
import org.scalatest.FunSuite

import java.text.ParseException
import java.{util => ju}

class QmarkTest extends FunSuite {
  test("recover from NullPointerException") {
    def expression = (null: String).length

    assertThrows[NullPointerException](expression)
    assert(0 == expression ? 0)
  }

  test("recover from NoSuchElementException") {
    def expression = ju.Arrays.asList[String]().anyElement

    assertThrows[NoSuchElementException](expression)
    assert("empty" == expression ? "empty")
  }

  test("recover from IndexOutOfBoundsException") {
    def expression = ju.Arrays.asList[String]()(0)

    assertThrows[IndexOutOfBoundsException](expression)
    assert("empty" == expression ? "empty")
  }

  test("recover from NumberFormatException") {
    def expression = "zero".toInt

    assertThrows[NumberFormatException]("zero".toInt)
    assert(0 == expression ? 0)
  }

  test("recover from IllegalArgumentException") {
    def expression = ju.Arrays.asList[Int]().mean

    assertThrows[IllegalArgumentException](ju.Arrays.asList[Int]().mean)
    assert(0 == expression ? 0)
  }

  test("recover from ParseException") {
    // it looks like java.text.ParseException is not thrown by native scex utils API
    def expression: JDate = throw new ParseException("parsing failed", 0)

    assertThrows[ParseException](expression)
    assert(new JDate(0) == expression ? new JDate(0))
  }

  test("recover from ExpressionRecoverableException") {
    class TestRecoverableException extends ExpressionRecoverableException

    def expression: String = throw new TestRecoverableException

    assertThrows[TestRecoverableException](expression)
    assert("recovered" == expression ? "recovered")
  }

  test("don't recover from generic RuntimeException") {
    def expression: String = throw new RuntimeException

    assertThrows[RuntimeException](expression)
    assertThrows[RuntimeException](expression ? "recovered")
  }
}
