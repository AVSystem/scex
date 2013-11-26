package com.avsystem.scex
package compiler.annotation

import java.{util => ju, lang => jl}
import scala.annotation.StaticAnnotation

/**
 * Ident and Select trees whose symbol is annotated with this annotation will not be validated, along with the
 * qualifier of Select. This is to mark symbols that server as always-available API inside expressions, like root object
 * or variable getters/setters.
 *
 * Created: 23-09-2013
 * Author: ghik
 */
class NotValidated extends StaticAnnotation
