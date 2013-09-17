package com.avsystem.scex.compiler

import java.{util => ju, lang => jl}

/**
 * Created: 17-09-2013
 * Author: ghik
 */
class XmlFriendlyJavaScexCompiler(val config: ScexCompilerConfig)
  extends XmlFriendlyScexCompiler with CachingScexCompiler with JavaScexCompilerApi
