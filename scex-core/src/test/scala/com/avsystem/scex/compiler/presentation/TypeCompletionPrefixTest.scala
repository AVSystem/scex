package com.avsystem.scex.compiler.presentation

import com.avsystem.commons.misc.TypeString
import com.avsystem.scex.Type
import com.avsystem.scex.compiler.CompilationTest
import com.avsystem.scex.compiler.presentation.TypeCompletionPrefixTest._
import com.avsystem.scex.compiler.presentation.ast.EmptyTree
import com.avsystem.scex.util.SimpleContext
import com.github.ghik.silencer.silent
import org.scalactic.source.Position
import org.scalatest.FunSuite

/**
  * Created: 07-10-2014
  * Author: ghik
  */
@silent("a pure expression does nothing in statement position")
class TypeCompletionPrefixTest extends FunSuite with CompilationTest with CompletionTest {

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
    on { str: String =>
      str.isEmpty
    }
  }

  private def createCompleter(acl: List[MemberAccessSpec], template: Boolean) =
    compiler.getCompleter[SimpleContext[Root], Any](createProfile(acl), template, header = header,
      variableTypes = Map("someInt" -> TypeString[Int]))

  private def assertPrefix(exprWithCaret: String, expectedPrefix: String, expectedType: Type, template: Boolean): Unit = {
    val exprWithSplice = if(!template) exprWithCaret else s"$${$exprWithCaret}"
    val completer = createCompleter(acl, template)
    val offset = exprWithSplice.indexOf('|') - 1
    val expr = exprWithSplice.substring(0, offset + 1) + exprWithSplice.substring(offset + 2)
    val completion = completer.getTypeCompletion(expr, offset)
    val prefixAttachments = completion.typedPrefixTree.attachments
    val tpe = prefixAttachments.tpe
    val prefixPos = prefixAttachments.position
    val prefix = expr.substring(prefixPos.start, prefixPos.end)

    assert(prefix == expectedPrefix)
    assert(tpe == expectedType)
  }

  private def assertNoPrefix(exprWithCaret: String, template: Boolean = false): Unit = {
    val completer = createCompleter(acl, template)
    val offset = exprWithCaret.indexOf('|') - 1
    val expr = exprWithCaret.substring(0, offset + 1) + exprWithCaret.substring(offset + 2)
    val completion = completer.getTypeCompletion(expr, offset)

    assert(completion.typedPrefixTree == EmptyTree)
    assert(completion.members.isEmpty)
  }

  private def tests(namePrefix: String, expectedPrefix: String, expectedType: Type)(exprs: String*)(implicit pos: Position): Unit = {
    exprs.foreach { exprWithCaret =>
      test(namePrefix + " " + exprWithCaret) {
        assertPrefix(exprWithCaret, expectedPrefix, expectedType, template = false)
        assertPrefix(exprWithCaret, expectedPrefix, expectedType, template = true)
      }
    }
  }

  private def tests(namePrefix: String)(exprs: String*)(implicit pos: Position): Unit = {
    exprs.foreach { exprWithCaret =>
      test(namePrefix + " " + exprWithCaret) {
        assertNoPrefix(exprWithCaret)
      }
    }
  }

  tests("literal")(
    "123|",
    "1|23"
  )

  tests("plain identifier")(
    "api|",
    "ap|i"
  )

  tests("variable reference")(
    "#|",
    "#lo|",
    "#lo|l",
    "#|lol"
  )

  tests("space after selection")(
    "api. |",
    "api.co |"
  )

  tests("inside argument list")(
    "api.aaa.substring(|",
    "api.aaa.substring(32|",
    "api.aaa.substring(32,|",
    "api.aaa.substring(32, |"
  )

  tests("plain selection", "api", scexType[Api])(
    "api.|",
    "api.aaa|",
    "api.aa|a",
    "api.|aaa",
    "api.aaa|.inc",
    "api.aa|a.inc",
    "api.|aaa.inc"
  )

  tests("plain selection with dangling dot", "api.aaa", scexType[String])(
    "api.aaa.inc|",
    "api.aaa.in|c",
    "api.aaa.|inc"
  )

  tests("incomplete selection", "api", scexType[Api])(
    "api.inc|",
    "api.i|nc",
    "api.|inc"
  )

  tests("incomplete selection accidentally keyword", "api", scexType[Api])(
    "api.type|",
    "api.typ|e",
    "api.t|ype",
    "api.|type"
  )

  tests("forbidden selection", "api", scexType[Api])(
    "api.zuo|",
    "api.z|uo",
    "api.|zuo"
  )

  tests("plain select dynamic", "dyn", scexType[Dyn])(
    "dyn.|",
    "dyn.whatevs|",
    "dyn.whatev|s",
    "dyn.|whatevs",
    "dyn.whatevs|.",
    "dyn.whatev|s.",
    "dyn.|whatevs.",
    "dyn.whatevs|.inc",
    "dyn.whatev|s.inc",
    "dyn.|whatevs.inc"
  )

  tests("select dynamic incomplete selection", "dyn.whatevs", scexType[Api])(
    "dyn.whatevs.|",
    "dyn.whatevs.inc|",
    "dyn.whatevs.i|nc",
    "dyn.whatevs.|inc"
  )

  tests("select dynamic incomplete subselection", "api.dynStr.lol", scexType[String])(
    "api.dynStr.lol.|",
    "api.dynStr.lol.is|",
    "api.dynStr.lol.i|s",
    "api.dynStr.lol.|is"
  )

  tests("argument-positioned select dynamic incomplete subselection", "api.dynStr.lol", scexType[String])(
    "api.dynStr.fuu + api.dynStr.lol.|",
    "api.dynStr.fuu + api.dynStr.lol.is|",
    "api.dynStr.fuu + api.dynStr.lol.i|s",
    "api.dynStr.fuu + api.dynStr.lol.|is"
  )

  tests("select dynamic complete subselection", "api.dynStr.lol", scexType[String])(
    "api.dynStr.lol.isEmpty|",
    "api.dynStr.lol.isEmpt|y",
    "api.dynStr.lol.isEmpty|.",
    "api.dynStr.lol.isEm|pty."
  )

  tests("select dynamic sub-subselection", "dyn.abc.aaa", scexType[String])(
    "dyn.abc.aaa.|",
    "dyn.abc.aaa.i|",
    "dyn.abc.aaa.i|s",
    "dyn.abc.aaa.|is",
    "dyn.abc.aaa.is|"
  )

  tests("double select dynamic", "api.dyn.fuu.dynStr", scexType[DynStr])(
    "api.dyn.fuu.dynStr.|",
    "api.dyn.fuu.dynStr.i|s",
    "api.dyn.fuu.dynStr.is|",
    "api.dyn.fuu.dynStr.|is"
  )

  tests("double select dynamic subselection", "api.dyn.fuu.dynStr.krap", scexType[String])(
    "api.dyn.fuu.dynStr.krap.|",
    "api.dyn.fuu.dynStr.krap.i|s",
    "api.dyn.fuu.dynStr.krap.is|",
    "api.dyn.fuu.dynStr.krap.|is"
  )

  tests("argument-positioned select dynamic complete subselection", "api.dynStr.lol", scexType[String])(
    "api.dynStr.fuu + api.dynStr.lol.isEmpty|",
    "api.dynStr.fuu + api.dynStr.lol.isEmpt|y",
    "api.dynStr.fuu + api.dynStr.lol.isEmpty|.",
    "api.dynStr.fuu + api.dynStr.lol.isEm|pty."
  )

  tests("argument-positioned double select dynamic", "api.dyn.fuu.dynStr", scexType[DynStr])(
    "api.dynStr.fuu + api.dyn.fuu.dynStr.|",
    "api.dynStr.fuu + api.dyn.fuu.dynStr.i|s",
    "api.dynStr.fuu + api.dyn.fuu.dynStr.is|",
    "api.dynStr.fuu + api.dyn.fuu.dynStr.|is"
  )

  tests("argument-positioned double select dynamic subselection", "api.dyn.fuu.dynStr.krap", scexType[String])(
    "api.dynStr.fuu + api.dyn.fuu.dynStr.krap.|",
    "api.dynStr.fuu + api.dyn.fuu.dynStr.krap.i|s",
    "api.dynStr.fuu + api.dyn.fuu.dynStr.krap.is|",
    "api.dynStr.fuu + api.dyn.fuu.dynStr.krap.|is"
  )

  tests("select dynamic forbidden selection", "dyn.whatevs", scexType[Api])(
    "dyn.whatevs.zuo|",
    "dyn.whatevs.z|uo",
    "dyn.whatevs.|zuo"
  )

  tests("empty param list method selection", "api", scexType[Api])(
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

  tests("typed dynamic variable member selection", "_vars.someInt", scexType[Int])(
    "_vars.someInt.|",
    "_vars.someInt.|lol",
    "_vars.someInt.l|ol"
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

    def dynStr: DynStr
  }

  trait ExtApi {
    def iii: String

    def zle: Int

    def dyn: Dyn
  }

  import scala.language.dynamics

  trait Dyn extends Dynamic {
    def selectDynamic(name: String): Api
  }

  trait DynStr extends Dynamic {
    def selectDynamic(name: String): String
  }

  implicit def apiToExt(api: Api): ExtApi = ???

}