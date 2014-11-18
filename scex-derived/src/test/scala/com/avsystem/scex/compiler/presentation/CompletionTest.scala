package com.avsystem.scex.compiler.presentation

import com.avsystem.scex.compiler.presentation.ScexPresentationCompiler.Type

import scala.reflect.runtime.{universe => ru}

/**
 * Author: ghik
 * Created: 11/18/14.
 */
trait CompletionTest {
  protected def scexType[T: ru.TypeTag] = {
    val tag = ru.typeTag[T]
    Type(tag.tpe.toString, tag.mirror.runtimeClass(tag.tpe.erasure))
  }

}
