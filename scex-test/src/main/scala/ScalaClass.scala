import java.{util => ju, lang => jl}

class ScalaClass(costam: Int) extends JavaClass(costam) {
  def this(s: String) = this(s.length)

  def this() = this(0)

  val stuff = 5
  var otherStuff = 10

  def stuff(lol: Int) = 42

  override def getSomeProperty = "scalaProperty"
}
