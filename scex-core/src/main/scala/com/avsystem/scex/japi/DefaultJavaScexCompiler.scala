package com.avsystem.scex.japi

import com.avsystem.scex.compiler.{CachingScexCompiler, ScexCompilerConfig}
import java.{util => ju, lang => jl}

/**
 * Created: 17-09-2013
 * Author: ghik
 */
class DefaultJavaScexCompiler(val config: ScexCompilerConfig)
  extends CachingScexCompiler with JavaScexCompiler