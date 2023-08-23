package com.avsystem.scex.presentation

import com.avsystem.scex.presentation.SymbolAttributes._
import com.avsystem.scex.symboldsl.SymbolInfo

object MathAttributes {
  final val scalaMathAttributes: List[SymbolInfo[Attributes]] = attributes {
    on { m: math.`package`.type =>
      m.IEEEremainder _ --> Attributes(documentation = "Returns the remainder resulting from the division of a specified number `x` by another specified number `y`.")

      m.abs(_: Double) --> Attributes(documentation = "Returns absolute value of `x` argument.")
      m.abs(_: Float) --> Attributes(documentation = "Returns absolute value of `x` argument.")
      m.abs(_: Int) --> Attributes(documentation = "Returns absolute value of `x` argument.")
      m.abs(_: Long) --> Attributes(documentation = "Returns absolute value of `x` argument.")

      m.acos _ --> Attributes(documentation = "Returns the arc cosine of the `x` argument.")

      m.addExact(_: Int, _: Int) --> Attributes(documentation = "Returns the sum of `x` and `y` arguments.")
      m.addExact(_: Long, _: Long) --> Attributes(documentation = "Returns the sum of `x` and `y` arguments.")

      m.asin _ --> Attributes(documentation = "Returns the arc sine of the `x` argument.")
      m.atan _ --> Attributes(documentation = "Returns the arc tangent of the `x` argument.")
      m.atan2 _ --> Attributes(documentation = "Returns the rectangular coordinates (x, y) converted to polar (r, theta).")

      m.cbrt _ --> Attributes(documentation = "Returns the cube root of the `x` argument.")
      m.ceil _ --> Attributes(documentation = "Returns the smallest double value that is greater than or equal to the `x` argument and is equal to a mathematical integer.")

      m.copySign(_: Double, _: Double) --> Attributes(documentation = "Returns the first argument `magnitude` with the sign of the second argument `sign`.")
      m.copySign(_: Float, _: Float) --> Attributes(documentation = "Returns the first argument `magnitude` with the sign of the second argument `sign`.")

      m.cos _ --> Attributes(documentation = "Returns the cosine of the `x` argument.")
      m.cosh _ --> Attributes(documentation = "Returns the hyperbolic cosine of the `x` argument.")

      m.decrementExact(_: Int) --> Attributes(documentation = "Returns the `x` argument decremented by one.")
      m.decrementExact(_: Long) --> Attributes(documentation = "Returns the `x` argument decremented by one.")

      m.exp _ --> Attributes(documentation = "Returns the Euler's number raised to the power of the `x` argument.")
      m.expm1 _ --> Attributes(documentation = "Returns `exp(x) - 1`, the Euler's number raised to the power of the `x` argument minus one.")

      m.floor _ --> Attributes(documentation = "Returns the largest integer value less than or equal to the `x` argument.")
      m.floorDiv(_: Int, _: Int) --> Attributes(documentation = "Returns the largest integer value less than or equal to the algebraic quotient, " + "where `x` is the dividend and `y` is the divisor.")
      m.floorDiv(_: Long, _: Long) --> Attributes(documentation = "Returns the largest integer value less than or equal to the algebraic quotient, " + "where `x` is the dividend and `y` is the divisor.")
      m.floorMod(_: Int, _: Int) --> Attributes(documentation = "Returns the floor modulus of the provided arguments, " + "where `x` is the dividend and `y` is the divisor.")
      m.floorMod(_: Long, _: Long) --> Attributes(documentation = "Returns the floor modulus of the provided arguments, " + "where `x` is the dividend and `y` is the divisor.")

      m.getExponent(_: Double) --> Attributes(documentation = "Returns the unbiased exponent used in the representation of the `d` argument.")
      m.getExponent(_: Float) --> Attributes(documentation = "Returns the unbiased exponent used in the representation of the `f` argument.")

      m.hypot _ --> Attributes(documentation = "Returns the square root of the sum of the squares of both `x` and `y` arguments without intermediate underflow or overflow.")

      m.incrementExact(_: Int) --> Attributes(documentation = "Returns the `x` argument incremented by one.")
      m.incrementExact(_: Long) --> Attributes(documentation = "Returns the `x` argument incremented by one.")

      m.log _ --> Attributes(documentation = "Returns natural logarithm of the `x` argument.")
      m.log10 _ --> Attributes(documentation = "Returns base 10 natural logarithm of the `x` argument.")
      m.log1p _ --> Attributes(documentation = "Returns the natural logarithm of the sum of the `x` argument and 1")

      m.max(_: Double, _: Double) --> Attributes(documentation = "Returns the greater of the provided `x` and `y` arguments.")
      m.max(_: Float, _: Float) --> Attributes(documentation = "Returns the greater of the provided `x` and `y` arguments.")
      m.max(_: Int, _: Int) --> Attributes(documentation = "Returns the greater of the provided `x` and `y` arguments.")
      m.max(_: Long, _: Long) --> Attributes(documentation = "Returns the greater of the provided `x` and `y` arguments.")

      m.min(_: Double, _: Double) --> Attributes(documentation = "Returns the smaller of the provided `x` and `y` arguments.")
      m.min(_: Float, _: Float) --> Attributes(documentation = "Returns the smaller of the provided `x` and `y` arguments.")
      m.min(_: Int, _: Int) --> Attributes(documentation = "Returns the smaller of the provided `x` and `y` arguments.")
      m.min(_: Long, _: Long) --> Attributes(documentation = "Returns the smaller of the provided `x` and `y` arguments.")

      m.multiplyExact(_: Int, _: Int) --> Attributes(documentation = "Returns the product of the `x` and `y` arguments.")
      m.multiplyExact(_: Long, _: Long) --> Attributes(documentation = "Returns the product of the `x` and `y` arguments.")

      m.negateExact(_: Int) --> Attributes(documentation = "Returns negation of the provided `x` argument.")
      m.negateExact(_: Long) --> Attributes(documentation = "Returns negation of the provided `x` argument.")

      m.nextAfter(_: Double, _: Double) --> Attributes(documentation = "Returns the floating-point number adjacent to the `start` argument in the direction of the `direction` argument.")
      m.nextAfter(_: Float, _: Double) --> Attributes(documentation = "Returns the floating-point number adjacent to the `start` argument in the direction of the `direction` argument.")
      m.nextDown(_: Double) --> Attributes(documentation = "Returns the floating-point value adjacent to the `d` argument in the direction of negative infinity.")
      m.nextDown(_: Float) --> Attributes(documentation = "Returns the floating-point value adjacent to the `f` argument in the direction of negative infinity.")
      m.nextUp(_: Double) --> Attributes(documentation = "Returns the floating-point value adjacent to the `d` argument in the direction of positive infinity.")
      m.nextUp(_: Float) --> Attributes(documentation = "Returns the floating-point value adjacent to the `f` argument in the direction of positive infinity.")

      m.pow(_: Double, _: Double) --> Attributes(documentation = "Returns the value of the `x` argument raised to the power of the the `y` argument. ")
      m.rint _ --> Attributes(documentation = "Returns the closest floating point value that is equal to a mathematical integer for the `x` argument.")

      m.round(_: Double) --> Attributes(documentation = "Returns the `x` argument rounded to the closest integer value.")
      m.round(_: Float) --> Attributes(documentation = "Returns the `x` argument rounded to the closest integer value.")
      m.round(_: Long) --> Attributes(documentation = "Returns the `x` argument rounded to the closest integer value.")

      m.scalb(_: Double, _: Int) --> Attributes(documentation = "Returns `d` times 2 to the power of `scaleFactor` " + "rounded as if performed by a single correctly rounded floating-point multiply.")
      m.scalb(_: Float, _: Int) --> Attributes(documentation = "Returns `f` times 2 to the power of `scaleFactor` " + "rounded as if performed by a single correctly rounded floating-point multiply.")

      m.signum(_: Double) --> Attributes(documentation = "Returns the signum function of the `x` argument.")
      m.signum(_: Float) --> Attributes(documentation = "Returns the signum function of the `x` argument.")
      m.signum(_: Int) --> Attributes(documentation = "Returns the signum function of the `x` argument.")
      m.signum(_: Long) --> Attributes(documentation = "Returns the signum function of the `x` argument.")

      m.sin _ --> Attributes(documentation = "Returns the sine of the `x` argument.")
      m.sinh _ --> Attributes(documentation = "Returns the hyperbolic sine of the `x` argument.")
      m.sqrt _ --> Attributes(documentation = "Returns the square root of the `x` argument.")

      m.subtractExact(_: Int, _: Int) --> Attributes(documentation = "Returns the difference of the `x` and `y` arguments.")
      m.subtractExact(_: Long, _: Long) --> Attributes(documentation = "Returns the difference of the `x` and `y` arguments.")

      m.tan _ --> Attributes(documentation = "Returns the tangent of the `x` argument.")
      m.tanh _ --> Attributes(documentation = "Returns the hyperbolic tangent of the `x` argument.")

      m.toDegrees _ --> Attributes(documentation = "Returns an angle measured in radians `x` converted to an approximately equivalent angle measured in degrees.")
      m.toRadians _ --> Attributes(documentation = "Returns an angle measured in degrees `x` converted to an approximately equivalent angle measured in radians.")

      m.ulp(_: Double) --> Attributes(documentation = "Returns the size of an ulp (unit in the last place) of the `x` argument.")
      m.ulp(_: Float) --> Attributes(documentation = "Returns the size of an ulp (unit in the last place) of the `x` argument.")
    }
  }
}
