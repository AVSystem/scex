package vaadin

import java.{lang => jl, util => ju}

/**
 * Created: 11-12-2013
 * Author: ghik
 */
class Root extends JavaRoot {
  def stuff = 5

  def javaLol = new JavaLol

  val tehList = List(1, 2, 3)
}

object Root {

  implicit class Costam(root: Root) {
    def dyn = Dyn
  }

}
