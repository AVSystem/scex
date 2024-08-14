package com.avsystem.scex.util

import com.avsystem.scex.presentation.annotation.Documentation
import org.apache.commons.lang3.time.DateUtils

import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoField
import java.time.{Clock, Instant, ZoneId, ZonedDateTime}
import java.util.{Calendar, Date}

final class EnrichedDate(wrapped: Date, zone: ZoneId = Clock.systemDefaultZone().getZone) {
  private def zonedDateTime: ZonedDateTime = ZonedDateTime.ofInstant(Instant.ofEpochMilli(wrapped.getTime), zone)

  @Documentation("Formats the date using the default date format: <tt>yyyy.MM.dd HH:mm:ss</tt>.")
  def format: String =
    if (zone == Clock.systemDefaultZone().getZone)
      CommonDateFormat.get.format(wrapped)
    else
      format(CommonDateFormat.get.toPattern)

  @Documentation("Formats the date according to provided date format. An example of correct date format is <tt>yyyy.MM.dd HH:mm:ss</tt>.")
  def format(dateFormat: String): String = DateTimeFormatter.ofPattern(dateFormat).format(zonedDateTime)

  @Documentation("Adds the provided amount of milliseconds to the date.")
  def addMilliseconds(amount: Int): Date = DateUtils.addMilliseconds(wrapped, amount)
  @Documentation("Adds the provided amount of seconds to the date.")
  def addSeconds(amount: Int): Date = DateUtils.addSeconds(wrapped, amount)
  @Documentation("Adds the provided amount of minutes to the date.")
  def addMinutes(amount: Int): Date = DateUtils.addMinutes(wrapped, amount)
  @Documentation("Adds the provided amount of hours to the date.")
  def addHours(amount: Int): Date = DateUtils.addHours(wrapped, amount)
  @Documentation("Adds the provided amount of days to the date.")
  def addDays(amount: Int): Date = DateUtils.addDays(wrapped, amount)
  @Documentation("Adds the provided amount of weeks to the date.")
  def addWeeks(amount: Int): Date = DateUtils.addWeeks(wrapped, amount)
  @Documentation("Adds the provided amount of months to the date.")
  def addMonths(amount: Int): Date = DateUtils.addMonths(wrapped, amount)
  @Documentation("Adds the provided amount of years to the date.")
  def addYears(amount: Int): Date = DateUtils.addYears(wrapped, amount)
  @Documentation("Truncates the date to seconds.")
  def truncateToSeconds: Date = DateUtils.truncate(wrapped, Calendar.SECOND)
  @Documentation("Truncates the date to minutes.")
  def truncateToMinutes: Date = DateUtils.truncate(wrapped, Calendar.MINUTE)
  @Documentation("Truncates the date to hours.")
  def truncateToHours: Date = DateUtils.truncate(wrapped, Calendar.HOUR)
  @Documentation("Truncates the date to days.")
  def truncateToDays: Date = DateUtils.truncate(wrapped, Calendar.DAY_OF_MONTH)
  @Documentation("Truncates the date to months.")
  def truncateToMonths: Date = DateUtils.truncate(wrapped, Calendar.MONTH)
  @Documentation("Truncates the date to years.")
  def truncateToYears: Date = DateUtils.truncate(wrapped, Calendar.YEAR)

  @Documentation("Returns the date as a number of milliseconds since 1970.01.01 00:00:00.")
  def millis: Long = wrapped.getTime

  @Documentation("Returns the seconds field of the date expressed in milliseconds.")
  def millisOfSecond: Int = zonedDateTime.get(ChronoField.MILLI_OF_SECOND)
  @Documentation("Returns the seconds field of the date.")
  def secondOfMinute: Int = zonedDateTime.get(ChronoField.SECOND_OF_MINUTE)
  @Documentation("Returns the number of seconds which passed since 00:00 for the date.")
  def secondOfDay: Int = zonedDateTime.get(ChronoField.SECOND_OF_DAY)
  @Documentation("Returns the minutes field of the date.")
  def minuteOfHour: Int = zonedDateTime.get(ChronoField.MINUTE_OF_HOUR)
  @Documentation("Returns the number of minutes which passed since 00:00 for the date.")
  def minuteOfDay: Int = zonedDateTime.get(ChronoField.MINUTE_OF_DAY)
  @Documentation("Returns the number of hours which passed since 00:00 for the date.")
  def hourOfDay: Int = zonedDateTime.get(ChronoField.HOUR_OF_DAY)
  @Documentation("Returns the day field of the date.")
  def dayOfMonth: Int = zonedDateTime.get(ChronoField.DAY_OF_MONTH)

  @Documentation("Returns a numeric value for day of the week of the date, where 1 - Monday, 7 - Sunday.")
  def dayOfWeek: Int = zonedDateTime.get(ChronoField.DAY_OF_WEEK)

  @Documentation("Returns a numeric value for the day of the year.")
  def dayOfYear: Int = zonedDateTime.get(ChronoField.DAY_OF_YEAR)

  @Documentation("Returns a numeric value for the month of the year, where 1 - January, 12 - December.")
  def monthOfYear: Int = zonedDateTime.get(ChronoField.MONTH_OF_YEAR)
}

