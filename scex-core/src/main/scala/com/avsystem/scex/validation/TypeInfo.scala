package com.avsystem.scex
package validation

import java.{util => ju, lang => jl}
import reflect.api.{Universe, TypeCreator}

class TypeInfo(typeCreator: TypeCreator, val clazz: Option[Class[_]], val isJava: Boolean, typeRepr: String) {

  def typeIn(u: Universe): u.Type =
    u.TypeTag[Any](u.rootMirror, typeCreator).tpe

  override def toString = typeRepr
}
