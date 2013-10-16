package com.avsystem.scex.japi

import com.avsystem.scex.compiler.{CachingScexCompiler, XmlFriendlyScexCompiler, ScexCompilerConfig}
import com.avsystem.scex.japi.JavaScexCompiler
import java.{util => ju, lang => jl}

/**
 * Created: 17-09-2013
 * Author: ghik
 */
class XmlFriendlyJavaScexCompiler(val config: ScexCompilerConfig)
  extends XmlFriendlyScexCompiler with CachingScexCompiler with JavaScexCompiler
