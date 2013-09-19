package com.avsystem.scex.compiler

import java.{util => ju, lang => jl}

/**
 * Created: 17-09-2013
 * Author: ghik
 */
class DefaultJavaScexCompiler(val config: ScexCompilerConfig) extends CachingScexCompiler with JavaScexCompiler