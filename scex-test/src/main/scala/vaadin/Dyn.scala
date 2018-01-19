package vaadin

import scala.language.dynamics

/**
  * Created: 10-12-2013
  * Author: ghik
  */
trait Dyn extends Dynamic {
  def selectDynamic(name: String): String

  def intermediate: Intermediate

  def intermediateMeth(str: Int): Intermediate

  def interDyn: InterDyn
}

trait InterDyn extends Dynamic {
  def selectDynamic(name: String): Intermediate

  def inter: Intermediate
}

trait Intermediate {
  def moreDyn: Dyn
}
