package com.avsystem.scex.compiler

import java.{util => ju, lang => jl}

/**
 * Created: 17-10-2013
 * Author: ghik
 */
class DefaultScexCompiler(val config: ScexCompilerConfig)
  extends ScexPresentationCompiler with CachingScexCompiler
