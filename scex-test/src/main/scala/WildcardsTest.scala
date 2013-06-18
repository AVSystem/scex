
object WildcardsTest {
  def main(args: Array[String]) {
    import com.avsystem.scex.validation.SymbolValidator._

    allow {
      on { sc: ScalaClass =>
        sc.all.members
      }
    } foreach println
    println()

    allow {
      on { sc: ScalaClass =>
        sc.all.constructors
      }
    } foreach println
    println()

    allow {
      on { sc: ScalaClass =>
        sc.all.declared.members
      }
    } foreach println
    println()

    allow {
      on { sc: ScalaClass =>
        sc.all.introduced.members
      }
    } foreach println
    println()

    allow {
      on { sc: ScalaClass =>
        sc.all.beanGetters
      }
    } foreach println
    println()

    allow {
      on { sc: ScalaClass =>
        sc.all.beanSetters
      }
    } foreach println
    println()

    allow {
      on { sc: ScalaClass =>
        sc.all.scalaGetters
      }
    } foreach println
    println()

    allow {
      on { sc: ScalaClass =>
        sc.all.scalaSetters
      }
    } foreach println
    println()

    allow {
      on { sc: ScalaClass =>
        sc.all.membersNamed.stuff
      }
    } foreach println
    println()

    allow {
      allStatic[String].members
    } foreach println
    println()

    allow {
      allStatic[String].membersNamed.valueOf
    } foreach println
    println()
  }
}
