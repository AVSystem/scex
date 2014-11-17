package com.avsystem.scex
package symboldsl

import java.{lang => jl, util => ju}

import scala.reflect.api.{TypeCreator, Universe}

class TypeInfo(typeCreator: TypeCreator, val clazz: Option[Class[_]], val isJava: Boolean, typeRepr: String) {

  def typeIn(u: Universe): u.Type =
    u.TypeTag[Any](u.rootMirror, typeCreator).tpe

  override def toString = typeRepr
}
