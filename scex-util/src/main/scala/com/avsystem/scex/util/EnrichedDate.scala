package com.avsystem.scex.util

import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

import com.avsystem.scex.presentation.annotation.Documentation
import org.apache.commons.lang3.time.DateUtils
import org.joda.time.DateTime

final class EnrichedDate(private val wrapped: Date) extends AnyVal {
  private def dateTime = new DateTime(wrapped.getTime)

  @Documentation("Formats the date using the default date format <tt>yyyy.MM.dd HH:mm:ss</tt>")
  def format: String = CommonDateFormat.get.format(wrapped)

  @Documentation("Formats the date according to passed date format. An example of correct date format is <tt>yyyy.MM.dd HH:mm:ss</tt>")
  def format(dateFormat: String): String = new SimpleDateFormat(dateFormat).format(wrapped)

  def addMilliseconds(amount: Int): Date = DateUtils.addMilliseconds(wrapped, amount)
  def addSeconds(amount: Int): Date = DateUtils.addSeconds(wrapped, amount)
  def addMinutes(amount: Int): Date = DateUtils.addMinutes(wrapped, amount)
  def addHours(amount: Int): Date = DateUtils.addHours(wrapped, amount)
  def addDays(amount: Int): Date = DateUtils.addDays(wrapped, amount)
  def addWeeks(amount: Int): Date = DateUtils.addWeeks(wrapped, amount)
  def addMonths(amount: Int): Date = DateUtils.addMonths(wrapped, amount)
  def addYears(amount: Int): Date = DateUtils.addYears(wrapped, amount)
  def truncateToSeconds: Date = DateUtils.truncate(wrapped, Calendar.SECOND)
  def truncateToMinutes: Date = DateUtils.truncate(wrapped, Calendar.MINUTE)
  def truncateToHours: Date = DateUtils.truncate(wrapped, Calendar.HOUR)
  def truncateToDays: Date = DateUtils.truncate(wrapped, Calendar.DAY_OF_MONTH)
  def truncateToMonths: Date = DateUtils.truncate(wrapped, Calendar.MONTH)
  def truncateToYears: Date = DateUtils.truncate(wrapped, Calendar.YEAR)

  @Documentation("Converts this date to number of milliseconds since 1970.01.01 00:00:00")
  def millis: Long = wrapped.getTime

  def millisOfSecond: Int = dateTime.getMillisOfSecond
  def secondOfMinute: Int = dateTime.getSecondOfMinute
  def secondOfDay: Int = dateTime.getSecondOfDay
  def minuteOfHour: Int = dateTime.getMinuteOfHour
  def minuteOfDay: Int = dateTime.getMinuteOfDay
  def hourOfDay: Int = dateTime.getHourOfDay
  def dayOfMonth: Int = dateTime.getDayOfMonth

  @Documentation("1 - Monday, 7 - Sunday")
  def dayOfWeek: Int = dateTime.getDayOfWeek

  def dayOfYear: Int = dateTime.getDayOfYear

  @Documentation("1 - January, 12 - December")
  def monthOfYear: Int = dateTime.getMonthOfYear
}

