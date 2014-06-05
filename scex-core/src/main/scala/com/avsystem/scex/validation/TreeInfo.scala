package com.avsystem.scex
package validation

import com.avsystem.scex.util.CommonUtils
import com.google.common.cache.CacheBuilder
import java.{util => ju, lang => jl}
import scala.reflect.api.{TypeCreator, Universe, TreeCreator}

/**
 * Created: 10-12-2013
 * Author: ghik
 */
class TreeInfo(treeCreator: TreeCreator, typeCreator: TypeCreator, val path: String) {

  import CommonUtils._

  // a double-entry cache (universe passed to treeIn will probably always be the compiler or presentation compiler)
  private val cache = CacheBuilder.newBuilder.weakKeys.maximumSize(2).build[Universe, Universe#Tree]

  def treeIn(u: Universe): u.Tree =
    cache.get(u, callable {
      u.Expr[Any](u.rootMirror, treeCreator)(u.TypeTag[Any](u.rootMirror, typeCreator)).tree
    }).asInstanceOf[u.Tree]

  override def toString = path
}
