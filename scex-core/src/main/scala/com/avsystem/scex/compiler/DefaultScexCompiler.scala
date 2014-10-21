package com.avsystem.scex
package compiler

import java.{util => ju, lang => jl}
import com.avsystem.scex.compiler.presentation.{CachingScexPresentationCompiler, ScexPresentationCompiler}

/**
 * Created: 17-10-2013
 * Author: ghik
 */
class DefaultScexCompiler extends ScexCompiler
  with ScexPresentationCompiler
  with CachingScexCompiler
  with CachingScexPresentationCompiler
  with WeakReferenceWrappingScexCompiler
  with LiteralsOptimizingScexCompiler

