package com.avsystem.scex
package japi

import java.{lang => jl, util => ju}

import com.avsystem.scex.compiler._
import com.avsystem.scex.compiler.presentation.{CachingScexPresentationCompiler, ScexPresentationCompiler}
import com.avsystem.scex.compiler.xmlfriendly.XmlFriendlyScexCompiler

/**
 * Created: 17-09-2013
 * Author: ghik
 */
class XmlFriendlyJavaScexCompiler(val settings: ScexSettings)
  extends ScexCompiler
  with ScexPresentationCompiler
  with ClassfileReusingScexCompiler
  with TemplateOptimizingScexCompiler
  with XmlFriendlyScexCompiler
  with CachingScexCompiler
  with CachingScexPresentationCompiler
  with WeakReferenceWrappingScexCompiler
  with JavaScexCompiler
