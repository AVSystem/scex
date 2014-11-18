import com.avsystem.scex.presentation.Attributes
import com.avsystem.scex.presentation.SymbolAttributes._

attributes {
  on { s: String =>
    s.charAt _ --> Attributes(
      paramNames = List("index"),
      documentation = "Returns character at specified index of the string (ranging from 0 to length-1)".stripMargin
    )
  }
}
