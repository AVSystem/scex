/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 */

package com.avsystem.scex.compiler

import scala.collection.mutable
import scala.reflect.io.{VirtualFile, AbstractFile}

/**
 * Code mostly copied from {@link scala.reflect.io.VirtualDirectory} just to implement remove ... :(
 */
class ScexVirtualDirectory(val name: String, maybeContainer: Option[ScexVirtualDirectory])
  extends AbstractFile {
  def path: String =
    maybeContainer match {
      case None => name
      case Some(parent) => parent.path + '/' + name
    }

  def absolute = this

  def container = maybeContainer.get

  def isDirectory = true

  var lastModified: Long = System.currentTimeMillis

  override def file = null

  override def input = sys.error("directories cannot be read")

  override def output = sys.error("directories cannot be written")

  def create() {
    unsupported()
  }

  def delete() {
    container.remove(name)
  }

  def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile = unsupported()

  private val files = mutable.Map.empty[String, AbstractFile]

  // the toList is so that the directory may continue to be
  // modified while its elements are iterated
  def iterator = files.values.toList.iterator

  override def lookupName(name: String, directory: Boolean): AbstractFile =
    (files get name filter (_.isDirectory == directory)).orNull

  override def fileNamed(name: String): AbstractFile =
    Option(lookupName(name, directory = false)) getOrElse {
      val newFile = new VirtualFile(name, path + '/' + name)
      files(name) = newFile
      newFile
    }

  override def subdirectoryNamed(name: String): AbstractFile =
    Option(lookupName(name, directory = true)) getOrElse {
      val dir = new ScexVirtualDirectory(name, Some(this))
      files(name) = dir
      dir
    }

  def remove(name: String) {
    files.remove(name)
  }

  def clear() {
    files.clear()
  }
}
