package vaadin

import java.{util => ju, lang => jl}

/**
 * Created: 11-12-2013
 * Author: ghik
 */
class Root {
  def stuff = 5

  val tehList = List(1, 2, 3)
}

object Root {

  implicit class Costam(root: Root) {
    def dyn = Dyn
  }

}
