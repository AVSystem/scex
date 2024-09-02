package com.avsystem.scex.util

import org.scalatest.funsuite.AnyFunSuite

import java.time.ZoneId
import java.util.{Calendar, Date}

class EnrichedDateTest extends AnyFunSuite {

  import CommonExpressionUtils.enrichDate

  // 2019-04-17 12:55:14.456 UTC
  val testDate = new Date(1555505714456L)

  // enrichZonedDate tested explicitly to provide stable zone for tests
  val testZonedDate: EnrichedZonedDate = EnrichedZonedDate.fromDate(testDate, ZoneId.of("Europe/Warsaw"))

  def localMidnight: Calendar = {
    val time = Calendar.getInstance()
    time.set(Calendar.HOUR_OF_DAY, 0)
    time.set(Calendar.MINUTE, 0)
    time.set(Calendar.SECOND, 0)
    time.set(Calendar.MILLISECOND, 0)
    time
  }

  test("addMilliseconds") {
    assert(testDate.addMilliseconds(1000) == new Date(1555505715456L))
  }

  test("addSeconds") {
    assert(testDate.addSeconds(1) == new Date(1555505715456L))
  }

  test("addMinutes") {
    assert(testDate.addMinutes(1) == new Date(1555505774456L))
  }

  test("addHours") {
    assert(testDate.addHours(1) == new Date(1555509314456L))
  }

  test("addDays") {
    assert(testDate.addDays(1) == new Date(1555592114456L))
  }

  test("addWeeks") {
    assert(testDate.addWeeks(1) == new Date(1556110514456L))
  }

  test("addMonths") {
    assert(testDate.addMonths(1) == new Date(1558097714456L))
  }

  // 2019-04-17 12:55:14.000 UTC
  test("truncate to seconds") {
    assert(testDate.truncateToSeconds == new Date(1555505714000L))
  }
  // 2019-04-17 12:55:00.000 UTC
  test("truncate to minutes") {
    assert(testDate.truncateToMinutes == new Date(1555505700000L))
  }
  // 2019-04-17 12:00:00.000 UTC
  test("truncate to hours") {
    assert(testDate.truncateToHours == new Date(1555502400000L))
  }
  // 2019-04-17 00:00:00.000 default time zone

  val truncatedToDays: Calendar = localMidnight
  truncatedToDays.set(Calendar.YEAR, 2019)
  truncatedToDays.set(Calendar.MONTH, Calendar.APRIL)
  truncatedToDays.set(Calendar.DAY_OF_MONTH, 17)
  test("truncate to day") {
    assert(testDate.truncateToDays == truncatedToDays.getTime)
  }
  // 2019-04-01 00:00:00.000 default time zone
  val truncatedToMonths: Calendar = localMidnight
  truncatedToMonths.set(Calendar.YEAR, 2019)
  truncatedToMonths.set(Calendar.MONTH, Calendar.APRIL)
  truncatedToMonths.set(Calendar.DAY_OF_MONTH, 1)
  test("truncate to month") {
    assert(testDate.truncateToMonths == truncatedToMonths.getTime)
  }
  // 2019-01-01 00:00:00.000 default time zone
  val truncatedToYears: Calendar = localMidnight
  truncatedToYears.set(Calendar.YEAR, 2019)
  truncatedToYears.set(Calendar.MONTH, Calendar.JANUARY)
  truncatedToYears.set(Calendar.DAY_OF_MONTH, 1)
  test("truncate to year") {
    assert(testDate.truncateToYears == truncatedToYears.getTime)
  }

  test("millis") {
    assert(testDate.millis == 1555505714456L)
  }

  test("format with default format") {
    assert(testZonedDate.format == "2019.04.17 14:55:14")
  }

  test("format with custom format") {
    assert(testZonedDate.format("yyyy-MM-dd") == "2019-04-17")
  }

  test("millisOfSecond") {
    assert(testZonedDate.millisOfSecond == 456)
  }

  test("secondOfMinute") {
    assert(testZonedDate.secondOfMinute == 14)
  }

  test("secondOfDay") {
    assert(testZonedDate.secondOfDay == 53714)
  }

  test("minuteOfHour") {
    assert(testZonedDate.minuteOfHour == 55)
  }

  test("minuteOfDay") {
    assert(testZonedDate.minuteOfDay == 895)
  }

  test("hourOfDay") {
    assert(testZonedDate.hourOfDay == 14)
  }

  test("dayOfMonth") {
    assert(testZonedDate.dayOfMonth == 17)
  }

  test("dayOfWeek") {
    assert(testZonedDate.dayOfWeek == 3)
  }

  test("dayOfYear") {
    assert(testZonedDate.dayOfYear == 107)
  }

  test("monthOfYear") {
    assert(testZonedDate.monthOfYear == 4)
  }

}
