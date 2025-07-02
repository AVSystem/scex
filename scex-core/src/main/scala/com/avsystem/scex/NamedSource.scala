package com.avsystem.scex

/**
 * Represents named source "file". Used to pass around compilable code fragments in runtime.
 *
 * Created: 29-10-2014
 * Author: ghik
 */
final case class NamedSource(name: String, code: String)
