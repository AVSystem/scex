package com.avsystem.scex

import com.avsystem.scex.validation.SymbolValidator
import java.{util => ju, lang => jl}
import scala.runtime._

object Utils {

  import SymbolValidator._

  def basicOperations = deny {
    // Methods on toplevel types
    on { any: Any =>
      any.equals _
      any.hashCode
      any.##
      any.asInstanceOf
      any.isInstanceOf
      any.getClass
    }
    on { anyRef: AnyRef =>
      anyRef.eq _
      anyRef.ne _
      anyRef.wait()
      anyRef.wait(_: Long)
      anyRef.wait(_: Long, _: Int)
      anyRef.notify()
      anyRef.notifyAll()
      anyRef.synchronized _
    }
  } ++ allow {
    on { any: Any =>
      any == _
      any != _
      any + (_: String)
      any -> (_: Any)
    }

    // Operations on primitive types
    on { u: Unit =>
      u.toString
      u.all.members
    }
    on { b: Boolean =>
      b.toString
      b.all.members
      b.implicitlyAs[RichBoolean].all.members
    }
    on { c: Char =>
      c.toString
      c.all.members
      c.implicitlyAs[RichChar].all.members
    }
    Char.char2double _
    Char.char2float _
    Char.char2int _
    Char.char2long _
    on { b: Byte =>
      b.toString
      b.all.members
      b.implicitlyAs[RichByte].all.members
    }
    Byte.byte2double _
    Byte.byte2float _
    Byte.byte2int _
    Byte.byte2long _
    Byte.byte2short _
    on { s: Short =>
      s.toString
      s.all.members
      s.implicitlyAs[RichShort].all.members
    }
    Short.short2double _
    Short.short2float _
    Short.short2int _
    Short.short2long _
    on { i: Int =>
      i.toString
      i.all.members
      i.implicitlyAs[RichInt].all.members
    }
    Int.int2double _
    Int.int2float _
    Int.int2long _
    on { l: Long =>
      l.toString
      l.all.members
      l.implicitlyAs[RichLong].all.members
    }
    Long.long2double _
    Long.long2float _
    on { f: Float =>
      f.toString
      f.all.members
      f.implicitlyAs[RichFloat].all.members
    }
    Float.float2double _
    on { d: Double =>
      d.toString
      d.all.members
      d.implicitlyAs[RichDouble].all.members
    }

    // Implicit conversions between primitive and boxed types
    Predef.Boolean2boolean _
    Predef.boolean2Boolean _
    Predef.Byte2byte _
    Predef.byte2Byte _
    Predef.Short2short _
    Predef.short2Short _
    Predef.Integer2int _
    Predef.int2Integer _
    Predef.Long2long _
    Predef.long2Long _
    Predef.Float2float _
    Predef.float2Float _
    Predef.Double2double _
    Predef.double2Double _

    // String related stuff
    StringContext.apply _
    on { sc: StringContext =>
      sc.s _
      sc.raw _
    }
    on { s: String =>
      s.charAt _
      s.codePointAt _
      s.codePointBefore _
      s.codePointCount _
      s.compareTo _
      s.compareToIgnoreCase _
      s.concat _
      s.contains _
      s.all.membersNamed.contentEquals
      s.endsWith _
      s.equalsIgnoreCase _
      s.all.membersNamed.indexOf
      s.isEmpty
      s.all.membersNamed.lastIndexOf
      s.length
      s.matches _
      s.offsetByCodePoints _
      s.all.membersNamed.regionMatches
      s.all.membersNamed.replace
      s.replaceAll _
      s.replaceAllLiterally(_: String, _: String)
      s.replaceFirst _
      s.all.membersNamed.split
      s.all.membersNamed.startsWith
      s.all.membersNamed.substring
      s.toLowerCase
      s.toUpperCase
      s.toString
      s.subSequence _
      s.trim
      s.implicitlyAs[Ordered[String]].all.members
      s.capitalize
      s.nonEmpty
      s.reverse
      s.stripLineEnd
      s.stripMargin
      s.stripMargin(_: Char)
      s.stripPrefix(_: String)
      s.stripSuffix(_: String)
      s.toBoolean
      s.toByte
      s.toShort
      s.toInt
      s.toLong
      s.toFloat
      s.toDouble
      s.apply(_: Int)
      s.slice(_: Int, _: Int)
    }

    // math functions
    math.`package`.all.members
  }
}
