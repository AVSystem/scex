package com.avsystem.scex.compiler

import com.avsystem.commons.jiop.JavaInterop._
import com.avsystem.scex.japi.ScalaTypeTokens.create

object ScalaTypeTokensTest {
  create[String]
  create[JList[String]]
  create[JList[_]]
}
