package com.avsystem.scex.compiler

import java.io.File

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
  nopredef.value = true

  private val Positive = Some((1, Int.MaxValue))

  final val expressionExpirationTime = IntSetting("-SCEXexpression-expiration-time",
    "Expiration time for expression cache, in seconds", 3600, Positive, _ => None)

  final val expressionCacheSize = IntSetting("-SCEXexpression-cache-size",
    "Maximum size of expression cache", 5000, Positive, _ => None)

  final val completionExpirationTime = IntSetting("-SCEXerrors-expiration-time",
    "Expiration time for completion caches, in seconds", 600, Positive, _ => None)

  final val errorsCacheSize = IntSetting("-SCEXerrors-cache-size",
    "Maximum size of errors cache", 10000, Positive, _ => None)

  final val scopeCompletionCacheSize = IntSetting("-SCEXscope-completion-cache-size",
    "Maximum size of scope completion cache", 3000, Positive, _ => None)

  final val typeMembersCacheSize = IntSetting("-SCEXtype-members-cache-size",
    "Maximum size of type members cache", 10000, Positive, _ => None)

  final val resetAfterCount = IntSetting("-SCEXreset-after-count",
    "Number of compilations after which the compiler will be reset", 2000, Positive, _ => None)

  final val classfileDirectory = StringSetting("-SCEXclassfile-directory", "directory",
    "Directory for classfile cache", "")

  final val noPresentation = BooleanSetting("-SCEXno-presentation",
    "Turns of the 'presentation' part of the compiler")

  final val noGetterAdapters = BooleanSetting("-SCEXno-getter-adapters",
    "Disables generation of Java getter adapter methods")

  final val backwardsCompatCacheVersion = StringSetting("-SCEXbackwards-compat-cache-version", "versionString",
    "Additional version string for controlling invalidation of classfile cache", "0")

  final val cacheUnexpectedCompilationExceptions = BooleanSetting("-SCEXcache-unexpected-compilation-exceptions",
    "Enables the caching of unexpected exceptions (such as NPE when accessing scex_classes) thrown during the expression compilation. " +
      "CompilationFailedExceptions are always cached, regardless of this setting. They indicate e.g. syntax errors, which should always be cached to avoid redundant compilations.", default = false)

  def resolvedClassfileDir: Option[PlainDirectory] = Option(classfileDirectory.value)
    .filter(_.trim.nonEmpty).map(path => new PlainDirectory(new Directory(new File(path))))
}
