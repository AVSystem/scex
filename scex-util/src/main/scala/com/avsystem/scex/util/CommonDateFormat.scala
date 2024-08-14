package com.avsystem.scex.util

import java.text.SimpleDateFormat

object CommonDateFormat {
  private val dateFormatTL = new ThreadLocal[SimpleDateFormat] {
    override def initialValue(): SimpleDateFormat = {
      val df = new SimpleDateFormat("yyyy.MM.dd HH:mm:ss")
      df.setLenient(false)
      df
    }
  }

  def get: SimpleDateFormat = dateFormatTL.get
}
