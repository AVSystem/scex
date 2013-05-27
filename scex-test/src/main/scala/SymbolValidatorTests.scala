import com.avsystem.scex.validation.SymbolValidator._
import java.{util => ju, lang => jl}


object SymbolValidatorTests {

  def main(args: Array[String]) {
    var specs: List[MemberAccessSpec] = null
    def printSpecs() {
      specs foreach println
      println()
    }

    specs = allow {
      on { t: Target =>
        t.all.methods
      }
    }
    printSpecs()

    specs = allow {
      on { t: Target =>
        t.all.declared.methods
      }
    }
    printSpecs()

    specs = allow {
      on { t: Target =>
        t.all.constructors
      }
    }
    printSpecs()

    specs = allow {
      on { t: Target =>
        t.all.scalaGetters
      }
    }
    printSpecs()

    specs = allow {
      on { t: Target =>
        t.all.scalaSetters
      }
    }
    printSpecs()

    specs = allow {
      on { t: Target =>
        t.all.beanGetters
      }
    }
    printSpecs()

    specs = allow {
      on { t: Target =>
        t.all.beanSetters
      }
    }
    printSpecs()

    specs = allow {
      on { t: Target =>
        t.all.methodsNamed.a
      }
    }
    printSpecs()

    specs = allow {
      on { t: Target =>
        t.all.methodsNamed.c
      }
    }
    printSpecs()
  }
}
