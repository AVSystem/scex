import com.avsystem.scex.TestMacros
import java.{util => ju, lang => jl}

object MacroTests {
  def main(args: Array[String]) {
    println(TestMacros.gimme(java.lang.String.CASE_INSENSITIVE_ORDER))
  }
}

class Stuff {
  val dafuq: Any = 5
}

object MTA extends Stuff {
  val fuu = 5

  override object dafuq

}
