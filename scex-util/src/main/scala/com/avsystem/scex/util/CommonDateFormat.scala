package com.avsystem.scex.util

import java.text.SimpleDateFormat
import java.time.format.DateTimeFormatter

// performance optimization, see https://github.com/AVSystem/scex/pull/38#discussion_r1724465654
object CommonDateFormat {
  final val DefaultPattern = "yyyy.MM.dd HH:mm:ss"

  final val Formatter: DateTimeFormatter = DateTimeFormatter.ofPattern(DefaultPattern)

  // thread-local instance of SimpleDateFormat because it's not thread-safe
  private val dateFormatTL = new ThreadLocal[SimpleDateFormat] {
    override def initialValue(): SimpleDateFormat = {
      val df = new SimpleDateFormat(DefaultPattern)
      df.setLenient(false)
      df
    }
  }

  def get: SimpleDateFormat = dateFormatTL.get
}
