package com.avsystem.scex.util

import com.avsystem.commons.jiop.JavaInterop._
import com.avsystem.scex.util.function._

import scala.annotation.nowarn
import scala.util.matching.Regex.Match

@nowarn("msg=a pure expression does nothing in statement position")
object CommonSymbolValidators {

  import com.avsystem.scex.validation.SymbolValidator._

  val javaCollectionExtensions = {
    import JavaCollectionExtensions._

    allow {
      on { c: JCollection[Any@plus] =>
        c.size
        c.contains _
        c.containsAll _
        c.isEmpty
        c.implicitlyAs[CollectionOps[Any]].all.members
      }
      on { sc: JCollection[String] =>
        sc.implicitlyAs[StringCollectionOps].all.members
      }
      list _
      range(_: Int, _: Int, _: Int)
      range(_: Int, _: Int)
      on { l: JList[Any@plus] =>
        l.get _
        l.implicitlyAs[ListOps[Any]].all.members
      }
      set _
      on { s: JSet[Any@plus] =>
        s.implicitlyAs[SetOps[Any]].all.members
      }
      map _
      on { m: JMap[Any@plus, Any@plus] =>
        m.isEmpty
        m.size
        m.get _
        m.keySet
        m.values
        m.containsKey _
        m.containsValue _
        m.implicitlyAs[MapOps[Any, Any]].all.introduced.members
      }
      entry _
      on { e: Entry[Any@plus, Any@plus] =>
        e.key
        e.value
        e.withKey _
        e.withValue _
        e.toString
      }
      on { c: JCollection[Entry[Any@plus, Any@plus]@plus] =>
        c.implicitlyAs[EntryCollectionOps[Any, Any]].all.members
      }
      on { c: JCollection[(Any, Any)@plus] =>
        c.implicitlyAs[PairCollectionOps[Any, Any]].all.members
      }

      on { o: Ordering.type =>
        o.BigDecimal
        o.BigInt
        o.Boolean
        o.Byte
        o.Char
        o.Double
        o.Float
        o.Int
        o.Long
        o.Short
        o.String
        o.Unit
        o.ordered(_: Nothing)
      }

      on { n: Numeric.type =>
        n.ByteIsIntegral
        n.CharIsIntegral
        n.ShortIsIntegral
        n.IntIsIntegral
        n.LongIsIntegral
        n.FloatIsFractional
        n.DoubleIsFractional
      }
    }
  }

  val commonExpressionApi = {
    import CommonExpressionUtils._

    deny {
      charOps
      exprUniversalOps

      on { b: Bytes =>
        b.bytes
      }
    } ++ allow {
      CommonExpressionUtils.all.members

      // Common types
      on { any: Any =>
        any ? (_: Any)
        any.useAs(_: Any => Any)
      }
      on { ch: Char =>
        ch.implicitlyAs[charOps].all.members
      }
      on { str: String =>
        str.getBytes
      }
      on { bl: JList[Byte] =>
        bl.implicitlyAs[byteListOps].all.members
      }
      on { b: Bytes =>
        b.all.introduced.members
      }
      on { d: JDate =>
        d.after _
        d.before _
        d.compareTo _
      }
      on { od: Ordered[JDate] =>
        od.all.members
      }
      dateSplicer.toString(_: JDate)
      collectionSplicer.toString(_: JSet[String])

      on { so: StringNetworkOps => so.all.introduced.members }
      on { so: StringMiscOps => so.all.introduced.members }
      on { ed: EnrichedDate => ed.all.introduced.members }
      on { ed: EnrichedZonedDate => ed.all.introduced.members }
      on { ea: EnrichedArray[_] => ea.all.introduced.members }

      on { su: StringUtil => su.all.members }
      on { nu: NetUtil => nu.all.members }
      on { fu: FormatUtil => fu.all.members }
      on { du: DateUtil => du.all.members }

      on { m: CommonExpressionUtils.math.type =>
        m.random()
      }
      on { m: Match =>
        m.group(_: Int)
        m.group(_: String)
        m.groupCount
        m.matched
      }
    }
  }

}
