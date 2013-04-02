/**
 * Created with IntelliJ IDEA.
 * User: ghik
 * Date: 08.01.13
 * Time: 21:03
 */
package object utils {

  implicit class EnhancedInt(val i: Int) extends AnyVal {
    def times(expr: => Any) {
      var c = 0
      while (c < i) {
        expr
        c += 1
      }
    }
  }

  implicit class EnhancedString(val str: String) extends AnyVal {
    def leftPad(w: Int) =
      if (str.length >= w)
        str.substring(0, w)
      else {
        str + " " * (str.length - w)
      }
  }

  def benchmark(expr: => Any): Double = {
    val start = System.nanoTime()
    expr
    (System.nanoTime() - start) / 1000000000.0
  }

}
