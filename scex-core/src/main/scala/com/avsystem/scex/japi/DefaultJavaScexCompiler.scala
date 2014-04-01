package com.avsystem.scex
package japi

import com.avsystem.scex.compiler.{LiteralsOptimizingScexCompiler, CachingScexCompiler, ScexCompilerConfig}
import java.{util => ju, lang => jl}
import com.avsystem.scex.compiler.presentation.{CachingScexPresentationCompiler, ScexPresentationCompiler}

/**
 * Created: 17-09-2013
 * Author: ghik
 */
class DefaultJavaScexCompiler(val config: ScexCompilerConfig)
  extends ScexCompilerConfig
  with ScexPresentationCompiler
  with CachingScexCompiler
  with CachingScexPresentationCompiler
  with LiteralsOptimizingScexCompiler
  with JavaScexCompiler
