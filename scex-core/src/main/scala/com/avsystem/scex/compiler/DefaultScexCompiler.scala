package com.avsystem.scex
package compiler

import java.{util => ju, lang => jl}
import com.avsystem.scex.compiler.presentation.{CachingScexPresentationCompiler, ScexPresentationCompiler}

/**
 * Created: 17-10-2013
 * Author: ghik
 */
class DefaultScexCompiler(val config: ScexCompilerConfig)
  extends ScexPresentationCompiler with CachingScexCompiler with CachingScexPresentationCompiler
