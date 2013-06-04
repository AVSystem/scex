import scala.runtime.RichInt

object WildcardsTest {
  def main(args: Array[String]) {
    import com.avsystem.scex.validation.SymbolValidator._

    allow {
      on { i: Int =>
        i.implicitlyAs[RichInt].all.methods
      }
    } foreach println
  }
}
