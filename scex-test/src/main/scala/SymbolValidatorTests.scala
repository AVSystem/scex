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
        t.anyMethod
      }
    }
    printSpecs()

    specs = allow {
      on { t: Target =>
        t.anyDeclaredMethod
      }
    }
    printSpecs()

    specs = allow {
      on { t: Target =>
        t.anyConstructor
      }
    }
    printSpecs()

    specs = allow {
      on { t: Target =>
        t.anyScalaGetter
      }
    }
    printSpecs()

    specs = allow {
      on { t: Target =>
        t.anyScalaSetter
      }
    }
    printSpecs()

    specs = allow {
      on { t: Target =>
        t.anyBeanGetter
      }
    }
    printSpecs()

    specs = allow {
      on { t: Target =>
        t.anyBeanSetter
      }
    }
    printSpecs()

    specs = allow {
      on { t: Target =>
        t.anyMethodNamed("a")
      }
    }
    printSpecs()

    specs = allow {
      on { t: Target =>
        t.anyMethodNamed("c")
      }
    }
    printSpecs()
  }
}
