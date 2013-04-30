package com.avsystem.scex.compiler.annotation

import java.{util => ju, lang => jl}
import scala.collection.mutable
import scala.collection.JavaConverters._
import scala.reflect.runtime.{universe => ru}
import scala.annotation.StaticAnnotation

class ContextAdapter extends StaticAnnotation
