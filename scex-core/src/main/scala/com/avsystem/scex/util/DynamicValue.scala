package com.avsystem.scex.util

import java.{util => ju, lang => jl}

/**
 * Simple wrapper type that serves as a generic, extensible union of some set of types.
 * By default, there are implicit conversions to DynamicValue from a set of simple types:
 * primitives, boxed primitives, Java enums and String.
 *
 * An existence of implicit conversion from some type T to DynamicValue represents the membership of
 * type T in the union. One can extend the union with more types by providing appropriate implicit conversions.
 *
 * Created: 15-11-2013
 * Author: ghik
 */
class DynamicValue(val wrapped: Any) extends AnyVal

object DynamicValue {
  implicit def wrap(wrapped: Boolean): DynamicValue =
    new DynamicValue(wrapped)

  implicit def wrap(wrapped: Char): DynamicValue =
    new DynamicValue(wrapped)

  implicit def wrap(wrapped: Short): DynamicValue =
    new DynamicValue(wrapped)

  implicit def wrap(wrapped: Int): DynamicValue =
    new DynamicValue(wrapped)

  implicit def wrap(wrapped: Long): DynamicValue =
    new DynamicValue(wrapped)

  implicit def wrap(wrapped: Float): DynamicValue =
    new DynamicValue(wrapped)

  implicit def wrap(wrapped: Double): DynamicValue =
    new DynamicValue(wrapped)

  implicit def wrap(wrapped: jl.Boolean): DynamicValue =
    new DynamicValue(wrapped)

  implicit def wrap(wrapped: jl.Character): DynamicValue =
    new DynamicValue(wrapped)

  implicit def wrap(wrapped: jl.Short): DynamicValue =
    new DynamicValue(wrapped)

  implicit def wrap(wrapped: jl.Integer): DynamicValue =
    new DynamicValue(wrapped)

  implicit def wrap(wrapped: jl.Long): DynamicValue =
    new DynamicValue(wrapped)

  implicit def wrap(wrapped: jl.Float): DynamicValue =
    new DynamicValue(wrapped)

  implicit def wrap(wrapped: jl.Double): DynamicValue =
    new DynamicValue(wrapped)

  implicit def wrap(wrapped: String): DynamicValue =
    new DynamicValue(wrapped)

  implicit def wrap(wrapped: jl.Enum[_]): DynamicValue =
    new DynamicValue(wrapped)
}
