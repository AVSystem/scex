package com.avsystem.scex
package japi

import com.avsystem.scex.compiler.{ScexCompiler, LiteralsOptimizingScexCompiler, CachingScexCompiler, ScexCompilerConfig}
import java.{util => ju, lang => jl}
import com.avsystem.scex.compiler.xmlfriendly.XmlFriendlyScexCompiler
import com.avsystem.scex.compiler.presentation.{CachingScexPresentationCompiler, ScexPresentationCompiler}

/**
 * Created: 17-09-2013
 * Author: ghik
 */
class XmlFriendlyJavaScexCompiler(val config: ScexCompilerConfig)
  extends ScexCompiler
  with ScexPresentationCompiler
  with XmlFriendlyScexCompiler
  with CachingScexCompiler
  with CachingScexPresentationCompiler
  with LiteralsOptimizingScexCompiler
  with JavaScexCompiler
