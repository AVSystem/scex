import com.avsystem.scex.validation.SymbolValidator._
import java.{lang => jl, util => ju}

allow {
  StringContext.apply _
  String.CASE_INSENSITIVE_ORDER
  Some.apply _
  String.valueOf(_: Boolean)

  on { c: JavaCostam[_] =>
    new JavaCostam(_: Nothing)
    c.getFuu
    c.toString
  }

  on { s: String =>
    s.length
    s.concat _
    s.matches _
    s.reverse
    s.compare(_: String)
    s.substring(_: Int)
  }

  on { sc: StringContext =>
    sc.s _
  }

  on { al: ju.ArrayList[_] =>
    al.constructorWithSignature("(x$1: java.util.Collection[_ <: E])java.util.ArrayList[E]")
  }

  on { any: Any =>
    any + (_: String)
    any -> (_: Any)
    any == (_: Any)
    any != (_: Any)
  }

  on { i: Int =>
    i.anyConstructor
    i.anyMethodNamed("+")
  }

} ++ deny {

  on { any: Any =>
    any.equals _
    any.hashCode
    any.##
    any.getClass
    any.asInstanceOf
    any.isInstanceOf
  }

  on { anyRef: AnyRef =>
    anyRef.eq _
    anyRef.synchronized _
  }

}