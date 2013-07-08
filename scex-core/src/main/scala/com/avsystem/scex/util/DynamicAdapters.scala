package com.avsystem.scex.util

import collection.JavaConversions
import collection.mutable
import java.util.Collections
import java.{util => ju, lang => jl}
import scala.language.dynamics


/**
 * Interfaces and utilities to allow Java implementations of {@link scala.Dynamic}.
 */
object DynamicAdapters {

  trait SelectAdapter[+R] extends Dynamic {
    def selectDynamic(member: String): R
  }

  trait UpdateAdapter[-A] extends Dynamic {
    def updateDynamic(member: String)(value: A)
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
     * Note: you can use {@link DynamicAdapters#varargsAsJavaList} to convert
     * <i>args</i> to {@link java.util.List}.
     */
    def applyDynamic(method: String)(args: AV*): R
  }

  /**
   * Converts Scala-style varargs (a {@link scala.collection.Seq}) into an unmodifiable {@link java.util.List}.
   */
  def varargsAsJavaList[AV](args: AV*) = JavaConversions.seqAsJavaList(args)

  trait ApplyNamedAdapter[-AV, +R] extends Dynamic {
    /**
     * Note: you can use {@link DynamicAdapters#namedArgsAsJavaMap} and
     * {@link DynamicAdapters#unnamedArgsAsJavaList} to extract named and unnamed
     * parameters from <i>args</i> as {@link java.util.Map} and {@link java.util.List}
     */
    def applyDynamicNamed(method: String)(args: (String, AV)*): R
  }

  /**
   * Extracts named arguments from named argument list and returns it
   * as unmodifiable {@link java.util.Map} with preserved order.
   *
   * E.g. for invocation <tt>obj.someDynamicMethod(a = 1, 2, b = 3, 4)</tt>, returned map will be
   * <tt>{a=1,b=3}</tt>
   */
  def namedArgsAsJavaMap[AV](args: (String, AV)*) = Collections.unmodifiableMap[String, AV](
    JavaConversions.mapAsJavaMap(mutable.LinkedHashMap(args.filter(_._1.nonEmpty): _*)))

  /**
   * Extracts unnamed argument values from named argument list and returns it as unmodifiable
   * {@link java.util.List}.
   *
   * E.g. for invocation <tt>obj.someDynamicMethod(a = 1, 2, b = 3, 4)</tt>, returned list will be
   * <tt>[2,4]</tt>
   */
  def unnamedArgsAsJavaList[AV](args: (String, AV)*) =
    JavaConversions.seqAsJavaList(args.collect { case ("", arg) => arg})
}
