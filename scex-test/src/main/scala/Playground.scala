
import java.{lang => jl, util => ju}

import scala.language.experimental.macros

object Playground {
  def main(args: Array[String]): Unit = {
    import com.avsystem.scex.TestMacros.SymbolStringDsl._

    strings {
      on { s: String =>
        s.offsetByCodePoints _ --> "this is offset by code points"
      }
    } foreach println

  }

}
