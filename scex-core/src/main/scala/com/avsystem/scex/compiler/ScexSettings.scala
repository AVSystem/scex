package com.avsystem.scex.compiler

import java.io.File
import java.{lang => jl, util => ju}

import scala.reflect.io.{Directory, PlainDirectory}
import scala.tools.nsc.Settings

/**
 * Created: 21-10-2014
 * Author: ghik
 */
class ScexSettings extends Settings {

  classpath.value = System.getProperty("java.class.path")
  exposeEmptyPackage.value = true
  // preserving 2.10 behaviour of macro expansion in presentation compiler
  // https://github.com/scala/scala/commit/6e4c926b4a4c5e8dd350ae3a150490a794b139ca
  // TODO: maybe try to make it work with MacroExpand.Discard ?
  Ymacroexpand.value = MacroExpand.Normal

  private val Positive = Some((1, Int.MaxValue))

  val expressionExpirationTime = IntSetting("-SCEXexpression-expiration-time",
    "Expiration time for expression cache, in seconds", 3600, Positive, _ => None)

  val expressionCacheSize = IntSetting("-SCEXexpression-cache-size",
    "Maximum size of expression cache", 5000, Positive, _ => None)

  val completionExpirationTime = IntSetting("-SCEXerrors-expiration-time",
    "Expiration time for completion caches, in seconds", 600, Positive, _ => None)

  val errorsCacheSize = IntSetting("-SCEXerrors-cache-size",
    "Maximum size of errors cache", 10000, Positive, _ => None)

  val scopeCompletionCacheSize = IntSetting("-SCEXscope-completion-cache-size",
    "Maximum size of scope completion cache", 3000, Positive, _ => None)

  val typeMembersCacheSize = IntSetting("-SCEXtype-members-cache-size",
    "Maximum size of type members cache", 10000, Positive, _ => None)

  val resetAfterCount = IntSetting("-SCEXreset-after-count",
    "Number of compilations after which the compiler will be reset", 2000, Positive, _ => None)

  val classfileDirectory = StringSetting("-SCEXclassfile-directory", "directory",
    "Directory for classfile cache", "")

  val noPresentation = BooleanSetting("-SCEXno-presentation",
    "Turns of the 'presentation' part of the compiler")

  val noGetterAdapters = BooleanSetting("-SCEXno-getter-adapters",
    "Disables generation of Java getter adapter methods")

  def resolvedClassfileDir = Option(classfileDirectory.value)
    .filter(_.trim.nonEmpty).map(path => new PlainDirectory(new Directory(new File(path))))
}
