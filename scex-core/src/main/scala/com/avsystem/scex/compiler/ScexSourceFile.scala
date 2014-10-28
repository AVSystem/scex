package com.avsystem.scex.compiler

import java.{util => ju, lang => jl}
import scala.collection.JavaConverters._
import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.{ClassTag, classTag}

/**
 * Created: 28-10-2014
 * Author: ghik
 */
class ScexSourceFile(name: String, contents: String, val shared: Boolean)
  extends BatchSourceFile(name, contents)
