package vaadin

import java.{lang => jl, util => ju}

import scala.language.dynamics

/**
 * Created: 10-12-2013
 * Author: ghik
 */
object Dyn extends Dynamic {
  def selectDynamic(name: String) = name
}

