package com.avsystem.scex
package japi

import com.avsystem.scex.compiler._
import java.{util => ju, lang => jl}
import com.avsystem.scex.compiler.xmlfriendly.XmlFriendlyScexCompiler
import com.avsystem.scex.compiler.presentation.{CachingScexPresentationCompiler, ScexPresentationCompiler}

/**
 * Created: 17-09-2013
 * Author: ghik
 */
class XmlFriendlyJavaScexCompiler(val settings: ScexSettings)
  extends ScexCompiler
  with ScexPresentationCompiler
  with ClassfilePersistingScexCompiler
  with LiteralsOptimizingScexCompiler
  with XmlFriendlyScexCompiler
  with CachingScexCompiler
  with CachingScexPresentationCompiler
  with WeakReferenceWrappingScexCompiler
  with JavaScexCompiler
