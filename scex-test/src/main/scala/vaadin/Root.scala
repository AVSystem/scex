package vaadin

/**
  * Created: 11-12-2013
  * Author: ghik
  */
trait Root extends JavaRoot {
  def stuff = 5

  def javaLol = new JavaLol

  val tehList = List(1, 2, 3)

  def goodyn: Dyn
}

object Root {

  implicit class Costam(root: Root) {
    def dyn: Dyn = ???
  }

}
