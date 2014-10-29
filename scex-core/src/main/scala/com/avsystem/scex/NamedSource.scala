package com.avsystem.scex

import java.{lang => jl, util => ju}

/**
 * Represents named source "file". Used to pass around compilable code fragments in runtime.
 *
 * Created: 29-10-2014
 * Author: ghik
 */
case class NamedSource(name: String, code: String)
