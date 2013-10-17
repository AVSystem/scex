package com.avsystem.scex.japi

import com.avsystem.scex.compiler.{ScexPresentationCompiler, CachingScexCompiler, XmlFriendlyScexCompiler, ScexCompilerConfig}
import java.{util => ju, lang => jl}

/**
 * Created: 17-09-2013
 * Author: ghik
 */
class XmlFriendlyJavaScexCompiler(val config: ScexCompilerConfig)
  extends ScexPresentationCompiler with XmlFriendlyScexCompiler with CachingScexCompiler with JavaScexCompiler
