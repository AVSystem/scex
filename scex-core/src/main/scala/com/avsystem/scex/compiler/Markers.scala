package com.avsystem.scex
package compiler

import java.{lang => jl, util => ju}

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
