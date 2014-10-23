package com.avsystem.scex
package japi

import java.{lang => jl, util => ju}

import com.avsystem.scex.compiler._
import com.avsystem.scex.compiler.presentation.{CachingScexPresentationCompiler, ScexPresentationCompiler}

/**
 * Created: 17-09-2013
 * Author: ghik
 */
class DefaultJavaScexCompiler(val settings: ScexSettings)
  extends ScexCompiler
  with ClassfilePersistingScexCompiler
  with ScexPresentationCompiler
  with CachingScexCompiler
  with CachingScexPresentationCompiler
  with WeakReferenceWrappingScexCompiler
  with LiteralsOptimizingScexCompiler
  with JavaScexCompiler
