package com.avsystem.scex
package compiler

import com.avsystem.scex.Macros
import java.{util => ju, lang => jl}
import scala.language.experimental.macros

/**
 * Created: 18-11-2013
 * Author: ghik
 */
trait TemplateInterpolations[T] {

  implicit class TemplateInterpolation(sc: StringContext) {
    def t(args: Any*): T = macro Macros.templateInterpolation_impl[T]
  }

}
