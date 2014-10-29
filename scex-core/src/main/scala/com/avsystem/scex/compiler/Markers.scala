package com.avsystem.scex
package compiler

import java.{util => ju, lang => jl}
import scala.collection.JavaConverters._
import scala.reflect.{ClassTag, classTag}

/**
 * Created: 12-12-2013
 * Author: ghik
 */
object Markers {

  trait Synthetic extends Any

  trait ExpressionUtil extends Any with Synthetic

  trait JavaGetterAdapter extends Any with Synthetic

  trait ProfileObject extends Any with Synthetic

}
