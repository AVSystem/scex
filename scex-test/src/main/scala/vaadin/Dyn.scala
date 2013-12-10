package vaadin

import java.{util => ju, lang => jl}
import scala.collection.JavaConverters._
import scala.reflect.{ClassTag, classTag}
import scala.language.dynamics

/**
 * Created: 10-12-2013
 * Author: ghik
 */
object Dyn extends Dynamic {
  def selectDynamic(name: String) = name
}

