package com.avsystem.scex.util

import java.text.SimpleDateFormat

// performance optimization, see https://github.com/AVSystem/scex/pull/38#discussion_r1724465654
object CommonDateFormat {
  // thread-local instance of SimpleDateFormat because it's not thread-safe
  private val dateFormatTL = new ThreadLocal[SimpleDateFormat] {
    override def initialValue(): SimpleDateFormat = {
      val df = new SimpleDateFormat("yyyy.MM.dd HH:mm:ss")
      df.setLenient(false)
      df
    }
  }

  def get: SimpleDateFormat = dateFormatTL.get
}
