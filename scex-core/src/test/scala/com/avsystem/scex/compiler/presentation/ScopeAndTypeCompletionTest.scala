package com.avsystem.scex.compiler.presentation

import com.avsystem.scex.compiler.presentation.ScexPresentationCompiler.Param
import com.avsystem.scex.compiler.presentation.ScopeAndTypeCompletionTest.{DynStr, Root, SvRoot}
import com.avsystem.scex.compiler.{CompilationTest, JavaRootWithGetter}
import com.avsystem.scex.presentation.Attributes
import com.avsystem.scex.presentation.annotation.Documentation
import com.avsystem.scex.util.SimpleContext
import org.scalatest.funsuite.AnyFunSuite

import scala.annotation.nowarn

/**
  * Author: ghik
  * Created: 11/18/14.
  */
@nowarn("msg=a pure expression does nothing in statement position")
class ScopeAndTypeCompletionTest extends AnyFunSuite with CompilationTest with CompletionTest {

  import com.avsystem.scex.util.CommonUtils._

  val acl = {
    import com.avsystem.scex.validation.SymbolValidator._
    allow {
      on { s: String =>
        s.charAt _
        s.toInt
        s.isEmpty
      }
      on { b: Boolean =>
        b.unary_!
      }
      on { r: Root =>
        r.method _
        r.com
      }
      on { jr: JavaRootWithGetter =>
        jr.getName
      }
      on { r: SvRoot =>
        r.sv
      }
      on { ds: DynStr =>
        ds.selectDynamic _
      }
    }
  }

  val attrs = {
    import com.avsystem.scex.presentation.SymbolAttributes._
    attributes {
      on { s: String =>
        s.charAt _ --> Attributes(documentation = "doc of charAt", paramNames = List("index"))
      }
    }
  }

  val utils =
    """
      |import com.avsystem.scex.presentation.annotation._
      |
      |implicit class rootOps(root: com.avsystem.scex.compiler.presentation.ScopeAndTypeCompletionTest.Root) {
      |  @Documentation("implicit method doc")
      |  def implicitMethod: Int = 42
      |}
      |
      |@Documentation("util stuff")
      |val utilStuff = 5
      |
    """.stripMargin

  val profile = createProfile(acl, attrs, utils = utils)

  test("simple type completion test") {
    val completer = compiler.getCompleter[SimpleContext[Unit], Any](profile, template = false)
    val completion = completer.getTypeCompletion("\"\".", 2).passTo(c => c.copy(members = c.members.sortBy(_.name)))

    assert(completion.members.map(asPartial) == Vector(
      PartialMember("charAt", scexType[Char], List(List(Param("index", scexType[Int]))), doc = "doc of charAt"),
      PartialMember("empty", scexType[Boolean]),
      PartialMember("isEmpty", scexType[Boolean], List(Nil)),
      PartialMember("toInt", scexType[Int])
    ))
  }

  test("attribute annotations test") {
    val completer = compiler.getCompleter[SimpleContext[Root], Any](profile, template = false)
    val completion = completer.getTypeCompletion("_root.", 5).passTo(c => c.copy(members = c.members.sortBy(_.name)))

    assert(completion.members.map(asPartial) === Vector(
      PartialMember("com", scexType[Int]),
      PartialMember("implicitMethod", scexType[Int], doc = "implicit method doc"),
      PartialMember("method", scexType[Any], List(List(
        Param("annotArg", scexType[Any]),
        Param("moar", scexType[Any])
      )), doc = "handles stuff")
    ))
  }

  test("simple scope completion test") {
    val completer = compiler.getCompleter[SimpleContext[Root], Any](profile, template = false)
    val completion = completer.getScopeCompletion.passTo(c => c.copy(members = c.members.sortBy(_.name)))

    assert(completion.members.filterNot(_.flags.iimplicit).map(asPartial) == Vector(
      PartialMember("com", scexType[Int]), // test shadowing of toplevel package
      PartialMember("method", scexType[Any], List(List(
        Param("annotArg", scexType[Any]),
        Param("moar", scexType[Any])
      )), doc = "handles stuff"),
      PartialMember("utilStuff", scexType[Int], doc = "util stuff")
    ))
  }

  test("scope completion test with adapted getters") {
    val completer = compiler.getCompleter[SimpleContext[JavaRootWithGetter], Any](profile, template = false)
    val completion = completer.getScopeCompletion.passTo(c => c.copy(members = c.members.sortBy(_.name)))

    assert(completion.members.filterNot(_.flags.iimplicit).map(asPartial) == Vector(
      PartialMember("getName", scexType[String], List(Nil)),
      PartialMember("name", scexType[String]),
      PartialMember("utilStuff", scexType[Int], doc = "util stuff")
    ))
  }

  test("type completion test with adapted getters") {
    val completer = compiler.getCompleter[SimpleContext[JavaRootWithGetter], Any](profile, template = false)
    val completion = completer.getTypeCompletion("_root.", 5).passTo(c => c.copy(members = c.members.sortBy(_.name)))

    assert(completion.members.map(asPartial) == Vector(
      PartialMember("getName", scexType[String], List(Nil)),
      PartialMember("name", scexType[String])
    ))
  }

  test("literal as Any") {
    val completer = compiler.getCompleter[SimpleContext[Unit], Any](profile, template = true)
    val errors = completer.getErrors("123")
    assert(errors.isEmpty)
  }
}

object ScopeAndTypeCompletionTest {

  trait Root {
    @Documentation("handles stuff")
    def method(annotArg: Any, moar: Any): Any
    def com: Int
  }

  trait SvRoot {
    def sv: DynStr
  }

  trait DynStr extends Dynamic {
    def selectDynamic(str: String): String
  }

}
