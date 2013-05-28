object WildcardsTest {
  def main(args: Array[String]) {
    import com.avsystem.scex.validation.SymbolValidator._

    allow {
      on { s: String =>
        s.all.introduced.methods
      }
    } foreach println
  }
}
