package com.avsystem.scex.compiler

import java.{util => ju, lang => jl}
import scala.collection.JavaConverters._
import scala.reflect.{ClassTag, classTag}

/**
 * Created: 12-12-2013
 * Author: ghik
 */
object Markers {
  trait ExpressionUtil extends Any
  trait JavaGetterAdapter extends Any
  trait ProfileObject extends Any
}
