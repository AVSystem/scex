package com.avsystem.scex.compiler

import java.{util => ju, lang => jl}

/**
 * Created: 17-09-2013
 * Author: ghik
 */
class JavaScexCompiler(val config: ScexCompilerConfig) extends CachingScexCompiler with JavaScexCompilerApi