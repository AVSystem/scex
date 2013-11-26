package com.avsystem.scex
package validation

import com.avsystem.scex.util.CommonUtils
import com.google.common.cache.CacheBuilder
import java.{util => ju, lang => jl}
import reflect.api.{Universe, TypeCreator}


class TypeInfo(typeCreator: TypeCreator, val clazz: Option[Class[_]], val isJava: Boolean, typeRepr: String) {

  import CommonUtils._

  // a double-entry cache (universe passed to typeIn will probably always be the compiler or presentation compiler)
  private val cache = CacheBuilder.newBuilder.weakKeys.maximumSize(2).build[Universe, Universe#Type]

  def typeIn(u: Universe): u.Type =
    cache.get(u, callable(u.TypeTag[Any](u.rootMirror, typeCreator).tpe)).asInstanceOf[u.Type]

  override def toString = typeRepr
}
