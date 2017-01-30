package com.avsystem.scex.util

import java.util.Collections
import java.{lang => jl, util => ju}

import scala.collection.mutable
import scala.collection.JavaConverters._
import scala.language.dynamics


/**
 * Interfaces and utilities to allow Java implementations of `scala.Dynamic`
 */
object DynamicAdapters {

  trait SelectAdapter[+R] extends Dynamic {
    def selectDynamic(member: String): R
  }

  trait UpdateAdapter[-A] extends Dynamic {
    def updateDynamic(member: String)(value: A): Unit
  }

  trait Apply0Adapter[+R] extends Dynamic {
    def applyDynamic(method: String): R
  }

  trait Apply1Adapter[-A1, +R] extends Dynamic {
    def applyDynamic(method: String)(arg1: A1): R
  }

  trait Apply2Adapter[-A1, -A2, +R] extends Dynamic {
    def applyDynamic(method: String)(arg1: A1, arg2: A2): R
  }

  trait Apply3Adapter[-A1, -A2, -A3, +R] extends Dynamic {
    def applyDynamic(method: String)(arg1: A1, arg2: A2, arg3: A3): R
  }

  trait Apply4Adapter[-A1, -A2, -A3, -A4, +R] extends Dynamic {
    def applyDynamic(method: String)(arg1: A1, arg2: A2, arg3: A3, arg4: A4): R
  }

  trait Apply5Adapter[-A1, -A2, -A3, -A4, -A5, +R] extends Dynamic {
    def applyDynamic(method: String)(arg1: A1, arg2: A2, arg3: A3, arg4: A4, arg5: A5): R
  }

  trait ApplyVarargsAdapter[-AV, +R] extends Dynamic {
    /**
     * Note: you can use `DynamicAdapters#varargsAsJavaList` to convert
     * <i>args</i> to `java.util.List`.
     */
    def applyDynamic(method: String)(args: AV*): R
  }

  /**
   * Converts Scala-style varargs (a `scala.collection.Seq`) into an unmodifiable `java.util.List`.
   */
  def varargsAsJavaList[AV](args: AV*) = args.asJava

  trait ApplyNamedAdapter[-AV, +R] extends Dynamic {
    /**
     * Note: you can use `DynamicAdapters#namedArgsAsJavaMap` and
     * `DynamicAdapters#unnamedArgsAsJavaList` to extract named and unnamed
     * parameters from <i>args</i> as `java.util.Map` and `java.util.List`
     */
    def applyDynamicNamed(method: String)(args: (String, AV)*): R
  }

  /**
   * Extracts named arguments from named argument list and returns it
   * as unmodifiable `java.util.Map` with preserved order.
   *
   * E.g. for invocation <tt>obj.someDynamicMethod(a = 1, 2, b = 3, 4)</tt>, returned map will be
   * <tt>{a=1,b=3}</tt>
   */
  def namedArgsAsJavaMap[AV](args: (String, AV)*) = Collections.unmodifiableMap[String, AV](
    mutable.LinkedHashMap(args.filter(_._1.nonEmpty): _*).asJava)

  /**
   * Extracts unnamed argument values from named argument list and returns it as unmodifiable
   * `java.util.List`.
   *
   * E.g. for invocation <tt>obj.someDynamicMethod(a = 1, 2, b = 3, 4)</tt>, returned list will be
   * <tt>[2,4]</tt>
   */
  def unnamedArgsAsJavaList[AV](args: (String, AV)*) =
    args.collect({ case ("", arg) => arg}).asJava

  /**
   * Convenience trait that can be implemented by contexts passed as expression inputs to provide
   * dynamic variable support.
   *
   * @tparam T
   */
  trait DynamicVariableSupport[T] extends Dynamic {
    def selectDynamic(name: String): T

    def updateDynamic(name: String)(value: T): Unit
  }

}
