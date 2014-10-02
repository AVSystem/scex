package vaadin

import java.{util => ju, lang => jl}
import scala.reflect.classTag
import scala.language.dynamics

/**
 * Created: 10-12-2013
 * Author: ghik
 */
object Dyn extends Dynamic {
  def selectDynamic(name: String) = name
}

