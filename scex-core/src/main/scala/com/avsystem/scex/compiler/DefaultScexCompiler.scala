package com.avsystem.scex
package compiler

import java.{lang => jl, util => ju}

import com.avsystem.scex.compiler.presentation.{CachingScexPresentationCompiler, ScexPresentationCompiler}

/**
 * Created: 17-10-2013
 * Author: ghik
 */
class DefaultScexCompiler
  extends ScexCompiler
  with ScexPresentationCompiler
  with ClassfilePersistingScexCompiler
  with CachingScexCompiler
  with CachingScexPresentationCompiler
  with WeakReferenceWrappingScexCompiler
  with LiteralsOptimizingScexCompiler

