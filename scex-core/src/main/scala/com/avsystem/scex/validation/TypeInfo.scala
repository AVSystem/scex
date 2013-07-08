package com.avsystem.scex.validation

import com.avsystem.scex.util.{CommonUtils, CacheImplicits}
import com.google.common.cache.CacheBuilder
import java.{util => ju, lang => jl}
import reflect.api.{Universe, TypeCreator}


class TypeInfo(typeCreator: TypeCreator, val clazz: Option[Class[_]], val isJava: Boolean, typeRepr: String) {

  import CommonUtils._

  // a single-entry cache (universe passed to typeIn will probably always be the compiler)
  private val cache = CacheBuilder.newBuilder.weakKeys.maximumSize(1)
    .initialCapacity(1).build[Universe, Universe#Type]

  def typeIn(u: Universe): u.Type =
    cache.get(u, callable(u.TypeTag[Any](u.rootMirror, typeCreator).tpe)).asInstanceOf[u.Type]

  override def toString = typeRepr
}
