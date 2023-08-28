package com.avsystem.scex.util

import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

import com.avsystem.scex.presentation.annotation.Documentation
import org.apache.commons.lang3.time.DateUtils
import org.joda.time.DateTime

final class EnrichedDate(private val wrapped: Date) extends AnyVal {
  private def dateTime = new DateTime(wrapped.getTime)

  @Documentation("Formats the date using the default date format: <tt>yyyy.MM.dd HH:mm:ss</tt>.")
  def format: String = CommonDateFormat.get.format(wrapped)

  @Documentation("Formats the date according to provided date format. An example of correct date format is <tt>yyyy.MM.dd HH:mm:ss</tt>.")
  def format(dateFormat: String): String = new SimpleDateFormat(dateFormat).format(wrapped)

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
  def millisOfSecond: Int = dateTime.getMillisOfSecond
  @Documentation("Returns the seconds field of the date.")
  def secondOfMinute: Int = dateTime.getSecondOfMinute
  @Documentation("Returns the number of seconds which passed since 00:00 for the date.")
  def secondOfDay: Int = dateTime.getSecondOfDay
  @Documentation("Returns the minutes field of the date.")
  def minuteOfHour: Int = dateTime.getMinuteOfHour
  @Documentation("Returns the number of minutes which passed since 00:00 for the date.")
  def minuteOfDay: Int = dateTime.getMinuteOfDay
  @Documentation("Returns the number of hours which passed since 00:00 for the date.")
  def hourOfDay: Int = dateTime.getHourOfDay
  @Documentation("Returns the day field of the date.")
  def dayOfMonth: Int = dateTime.getDayOfMonth

  @Documentation("Returns a numeric value for day of the week of the date, where 1 - Monday, 7 - Sunday.")
  def dayOfWeek: Int = dateTime.getDayOfWeek

  @Documentation("Returns a numeric value for the day of the year.")
  def dayOfYear: Int = dateTime.getDayOfYear

  @Documentation("Returns a numeric value for the month of the year, where 1 - January, 12 - December.")
  def monthOfYear: Int = dateTime.getMonthOfYear
}

