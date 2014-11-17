package com.avsystem.scex.compiler.presentation

import java.{lang => jl, util => ju}

import com.avsystem.scex.compiler.presentation.TypeCompletionPrefixTest._
import com.avsystem.scex.compiler.{CompilationTest, ScexFunSuite}
import com.avsystem.scex.util.SimpleContext

import scala.reflect.runtime.{universe => ru}

/**
 * Created: 07-10-2014
 * Author: ghik
 */
class TypeCompletionPrefixTest extends ScexFunSuite with CompilationTest {

  import com.avsystem.scex.compiler.presentation.ScexPresentationCompiler._
  import com.avsystem.scex.validation.SymbolValidator._

  private val header =
    """
      |import com.avsystem.scex.compiler.TypeCompletionTest._
    """.stripMargin

  private val acl = deny {
    on { api: Api =>
      api.zuo
      api.zle
    }
  } ++ allow {
    on { r: Root =>
      r.all.members
    }
    on { api: Api =>
      api.all.members
      api.implicitlyAs[ExtApi].all.members
    }
    on { dyn: Dyn =>
      dyn.all.members
    }
  }

  private def createCompleter(acl: List[MemberAccessSpec]) =
    compiler.getCompleter[SimpleContext[Root], Any](createProfile(acl), template = false, header = header)

  private def scexType[T: ru.TypeTag] = {
    val tag = ru.typeTag[T]
    Type(tag.tpe.toString, tag.mirror.runtimeClass(tag.tpe))
  }

  private def assertPrefix(exprWithCaret: String, expectedPrefix: String, expectedType: Type): Unit = {
    val completer = createCompleter(acl)
    val offset = exprWithCaret.indexOf('|') - 1
    val expr = exprWithCaret.substring(0, offset + 1) + exprWithCaret.substring(offset + 2)
    val completion = completer.getTypeCompletion(expr, offset)
    val prefixAttachments = completion.typedPrefixTree.attachments
    val tpe = prefixAttachments.tpe
    val prefixPos = prefixAttachments.position
    val prefix = expr.substring(prefixPos.start, prefixPos.end)

    assert(prefix === expectedPrefix)
    assert(tpe === expectedType)
  }

  private case class TestCase(expr: String, expectedPrefix: String, expectedType: Type)

  private def tests(namePrefix: String, expectedPrefix: String, expectedType: Type)(exprs: String*): Unit = {
    exprs.foreach { exprWithCaret =>
      test(namePrefix + " " + exprWithCaret) {
        assertPrefix(exprWithCaret, expectedPrefix, expectedType)
      }
    }
  }

  tests("plain identifier", "api", scexType[Api])(
    "api|",
    "ap|i"
  )

  tests("dangling dot", "api", scexType[Api])(
    "api.|",
    "api|.",
    "ap|i."
  )

  tests("plain selection", "api.aaa", scexType[String])(
    "api.aaa|",
    "api.aa|a",
    "api.|aaa"
  )

  tests("plain selection with dangling dot", "api.aaa", scexType[String])(
    "api.aaa.inc|",
    "api.aaa.in|c",
    "api.aaa.|inc",
    "api.aaa|.inc",
    "api.aa|a.inc",
    "api.|aaa.inc"
  )

  tests("plain selection prefix", "api", scexType[Api])(
    "api|.aaa",
    "ap|i.aaa"
  )

  tests("incomplete selection", "api", scexType[Api])(
    "api.inc|",
    "api.i|nc",
    "api.|inc",
    "api|.inc",
    "ap|i.inc"
  )

  tests("forbidden selection", "api", scexType[Api])(
    "api.zuo|",
    "api.z|uo",
    "api.|zuo",
    "api|.zuo",
    "ap|i.zuo"
  )

  tests("plain select dynamic", "dyn.whatevs", scexType[Api])(
    "dyn.whatevs|",
    "dyn.whatev|s",
    "dyn.|whatevs"
  )

  tests("select dynamic dangling dot", "dyn.whatevs", scexType[Api])(
    "dyn.whatevs.|",
    "dyn.whatevs|.",
    "dyn.whatev|s.",
    "dyn.|whatevs."
  )

  tests("select dynamic incomplete selection", "dyn.whatevs", scexType[Api])(
    "dyn.whatevs.inc|",
    "dyn.whatevs.i|nc",
    "dyn.whatevs.|inc",
    "dyn.whatevs|.inc",
    "dyn.whatev|s.inc",
    "dyn.|whatevs.inc"
  )

  tests("select dynamic forbidden selection", "dyn.whatevs", scexType[Api])(
    "dyn.whatevs.zuo|",
    "dyn.whatevs.z|uo",
    "dyn.whatevs.|zuo",
    "dyn.whatevs|.zuo",
    "dyn.whatev|s.zuo",
    "dyn.|whatevs.zuo"
  )

  tests("empty param list method selection", "api.ccc", scexType[Int])(
    "api.ccc.|",
    "api.ccc|.",
    "api.ccc|",
    "api.cc|c",
    "api.|ccc"
  )

  tests("forbidden implicit member selection", "api", scexType[Api])(
    "api.zle|",
    "api.zl|e",
    "api.|zle"
  )

}

object TypeCompletionPrefixTest {

  trait Root {
    def api: Api

    def dyn: Dyn
  }

  trait Api {
    def aaa: String

    def bbb: Int

    def ccc(): Int

    def ddd(a: Int): Int

    def zuo: Int
  }

  trait ExtApi {
    def iii: String

    def zle: Int
  }

  import scala.language.dynamics

  trait Dyn extends Dynamic {
    def selectDynamic(name: String): Api
  }

  implicit def apiToExt(api: Api): ExtApi = ???

}