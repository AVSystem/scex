package com.avsystem.scex
package symboldsl

import java.{lang => jl, util => ju}

import com.google.common.cache.CacheBuilder

import scala.reflect.api.{TreeCreator, TypeCreator, Universe}

/**
 * Created: 10-12-2013
 * Author: ghik
 */
class TreeInfo(treeCreator: TreeCreator, typeCreator: TypeCreator, val path: String) {

  import com.avsystem.scex.util.CommonUtils._

  // a double-entry cache (universe passed to treeIn will probably always be the compiler or presentation compiler)
  private val cache = CacheBuilder.newBuilder.weakKeys.maximumSize(2).build[Universe, Universe#Tree]

  def treeIn(u: Universe): u.Tree =
    cache.get(u, callable {
      u.Expr[Any](u.rootMirror, treeCreator)(u.TypeTag[Any](u.rootMirror, typeCreator)).tree
    }).asInstanceOf[u.Tree]

  override def toString = path
}
