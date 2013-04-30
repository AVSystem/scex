package com.avsystem.scex.compiler.annotation

import java.{util => ju, lang => jl}
import scala.annotation.StaticAnnotation
import scala.reflect.runtime.{universe => ru}

class BooleanIsGetter extends StaticAnnotation