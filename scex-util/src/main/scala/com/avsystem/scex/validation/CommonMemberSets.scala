package com.avsystem.scex.validation

import java.{util => ju, lang => jl}
import scala.language.experimental.macros
import scala.runtime._

object CommonMemberSets {

  import SymbolValidator._

  def basicSymbolsProfile = deny {
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
      u.all.members
    }
    on { b: Boolean =>
      b.all.members
      b.implicitlyAs[RichBoolean].all.members
    }
    on { c: Char =>
      c.all.members
      c.implicitlyAs[RichChar].all.members
    }
    on { b: Byte =>
      b.all.members
      b.implicitlyAs[RichByte].all.members
    }
    on { s: Short =>
      s.all.members
      s.implicitlyAs[RichShort].all.members
    }
    on { i: Int =>
      i.all.members
      i.implicitlyAs[RichInt].all.members
    }
    on { l: Long =>
      l.all.members
      l.implicitlyAs[RichLong].all.members
    }
    on { f: Float =>
      f.all.members
      f.implicitlyAs[RichFloat].all.members
    }
    on { d: Double =>
      d.all.members
      d.implicitlyAs[RichDouble].all.members
    }
  }

  def main(args: Array[String]) {
    basicSymbolsProfile foreach println
  }
}
