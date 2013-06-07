import scala.beans.BeanProperty

class Target {
  var a = 5
  val b = 10

  def c = 20

  def c_=(_c: Int) {}

  def c(dafuq: Int) = "trololo"

  def a(aa: String) {}

  @BeanProperty var d = 30

  def getE = 40

  def getF() = 50

  def isG = 60

  def isH() = 70

  def isI = true

  def isJ() = true
}

object OTarget extends Target {

  object Inner {
    def lol = 5
  }

}