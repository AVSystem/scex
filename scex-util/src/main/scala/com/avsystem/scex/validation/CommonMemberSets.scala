package com.avsystem.scex.validation

import java.{util => ju, lang => jl}
import scala.language.experimental.macros
import scala.runtime._

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
      b.implicitlyAs[RichBoolean].all.members
    }
    on { c: Char =>
      c.all.introduced.members
      c.implicitlyAs[RichChar].all.members
    }
    on { b: Byte =>
      b.all.introduced.members
      b.implicitlyAs[RichByte].all.members
    }
    on { s: Short =>
      s.all.introduced.members
      s.implicitlyAs[RichShort].all.members
    }
    on { i: Int =>
      i.all.introduced.members
      i.implicitlyAs[RichInt].all.members
    }
    on { l: Long =>
      l.all.introduced.members
      l.implicitlyAs[RichLong].all.members
    }
    on { f: Float =>
      f.all.introduced.members
      f.implicitlyAs[RichFloat].all.members
    }
    on { d: Double =>
      d.all.introduced.members
      d.implicitlyAs[RichDouble].all.members
    }
  }

  def main(args: Array[String]) {
    basicOperationsProfile foreach println
  }
}
