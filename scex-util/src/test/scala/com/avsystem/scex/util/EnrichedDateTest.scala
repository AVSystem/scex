package com.avsystem.scex.util

import java.util.{Calendar, Date}

import org.scalatest.FunSuite

class EnrichedDateTest extends FunSuite {
  // 2019-04-17 12:55:14.456 UTC
  val testDate: EnrichedDate = new EnrichedDate(new Date(1555505714456L))

  def localMidnight: Calendar = {
    val time = Calendar.getInstance()
    time.set(Calendar.HOUR_OF_DAY, 0)
    time.set(Calendar.MINUTE, 0)
    time.set(Calendar.SECOND, 0)
    time.set(Calendar.MILLISECOND, 0)
    time
  }

  // 2019-04-17 12:55:14.000 UTC
  test("truncate to seconds")(assert(testDate.truncateToSeconds === new Date(1555505714000L)))
  // 2019-04-17 12:55:00.000 UTC
  test("truncate to minutes")(assert(testDate.truncateToMinutes === new Date(1555505700000L)))
  // 2019-04-17 12:00:00.000 UTC
  test("truncate to hours")(assert(testDate.truncateToHours === new Date(1555502400000L)))
  // 2019-04-17 00:00.00.000 default time zone
  val truncatedToDays: Calendar = localMidnight
  truncatedToDays.set(Calendar.YEAR, 2019)
  truncatedToDays.set(Calendar.MONTH, Calendar.APRIL)
  truncatedToDays.set(Calendar.DAY_OF_MONTH, 17)
  test("truncate to day")(assert(testDate.truncateToDays === truncatedToDays.getTime))
  // 2019-04-01 00:00.00.000 default time zone
  val truncatedToMonths: Calendar = localMidnight
  truncatedToMonths.set(Calendar.YEAR, 2019)
  truncatedToMonths.set(Calendar.MONTH, Calendar.APRIL)
  truncatedToMonths.set(Calendar.DAY_OF_MONTH, 1)
  test("truncate to month")(assert(testDate.truncateToMonths === truncatedToMonths.getTime))
  // 2019-01-01 00:00.00.000 default time zone
  val truncatedToYears: Calendar = localMidnight
  truncatedToYears.set(Calendar.YEAR, 2019)
  truncatedToYears.set(Calendar.MONTH, Calendar.JANUARY)
  truncatedToYears.set(Calendar.DAY_OF_MONTH, 1)
  test("truncate to year")(assert(testDate.truncateToYears === truncatedToYears.getTime))
}
