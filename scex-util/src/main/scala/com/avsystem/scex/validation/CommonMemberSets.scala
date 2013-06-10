package com.avsystem.scex.validation

import java.{util => ju, lang => jl}
import scala.language.experimental.macros

object CommonMemberSets {

  import SymbolValidator._

  def basicOperationsProfile = deny {
    on { any: Any =>
      any.equals _
      any.hashCode
      any.##
      any.asInstanceOf
      any.isInstanceOf
      any.getClass
    }
  } ++ allow {
    on { any: Any =>
      any.== _
      any.!= _
    }
    on { u: Unit =>
      u.all.introduced.members
    }
    on { b: Boolean =>
      b.all.introduced.members
    }
    on { c: Char =>
      c.all.introduced.members
    }
    on { b: Byte =>
      b.all.introduced.members
    }
    on { s: Short =>
      s.all.introduced.members
    }
    on { i: Int =>
      i.all.introduced.members
    }
    on { l: Long =>
      l.all.introduced.members
    }
    on { f: Float =>
      f.all.introduced.members
    }
    on { d: Double =>
      d.all.introduced.members
    }
  }

  def main(args: Array[String]) {
    basicOperationsProfile foreach println
  }
}
