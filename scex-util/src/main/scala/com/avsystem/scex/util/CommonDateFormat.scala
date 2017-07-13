package com.avsystem.scex.util

import java.text.{DateFormat, SimpleDateFormat}

object CommonDateFormat {
  private val dateFormatTL = new ThreadLocal[DateFormat] {
    override def initialValue(): DateFormat = {
      val df = new SimpleDateFormat("yyyy.MM.dd HH:mm:ss")
      df.setLenient(false)
      df
    }
  }

  def get: DateFormat = dateFormatTL.get
}
