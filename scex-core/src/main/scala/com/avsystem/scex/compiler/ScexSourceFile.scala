package com.avsystem.scex.compiler

import scala.reflect.internal.util.BatchSourceFile

/**
 * Created: 28-10-2014
 * Author: ghik
 */
class ScexSourceFile(name: String, contents: String, val shared: Boolean)
  extends BatchSourceFile(name, contents) {

  override def equals(that: Any): Boolean = that match {
    case that: ScexSourceFile => file.path == that.file.path && start == that.start
    case _ => super.equals(that)
  }

  override def hashCode: Int = file.path.## + start.##

}
