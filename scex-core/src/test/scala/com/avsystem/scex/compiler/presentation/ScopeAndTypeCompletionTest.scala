package com.avsystem.scex.compiler.presentation

import com.avsystem.scex.compiler.presentation.ScexPresentationCompiler.Param
import com.avsystem.scex.compiler.presentation.ScopeAndTypeCompletionTest.{DynStr, Root, SvRoot}
import com.avsystem.scex.compiler.{CompilationTest, JavaRootWithGetter}
import com.avsystem.scex.presentation.Attributes
import com.avsystem.scex.presentation.annotation.{Documentation, ParameterNames}
import com.avsystem.scex.util.SimpleContext
import org.scalatest.FunSuite

/**
  * Author: ghik
  * Created: 11/18/14.
  */
class ScopeAndTypeCompletionTest extends FunSuite with CompilationTest with CompletionTest {

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
    val completion = completer.getTypeCompletion("\"\"", 1).passTo(c => c.copy(members = c.members.sortBy(_.name)))

    assert(completion.members.map(asPartial) == Vector(
      PartialMember("charAt", scexType[Char], List(List(Param("index", scexType[Int]))), doc = "doc of charAt"),
      PartialMember("empty", scexType[Boolean]),
      PartialMember("isEmpty", scexType[Boolean], List(Nil)),
      PartialMember("toInt", scexType[Int])
    ))
  }

  test("attribute annotations test") {
    val completer = compiler.getCompleter[SimpleContext[Root], Any](profile, template = false)
    val completion = completer.getTypeCompletion("_root", 4).passTo(c => c.copy(members = c.members.sortBy(_.name)))

    assert(completion.members.map(asPartial) == Vector(
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
    val completion = completer.getTypeCompletion("_root", 4).passTo(c => c.copy(members = c.members.sortBy(_.name)))

    assert(completion.members.map(asPartial) == Vector(
      PartialMember("getName", scexType[String], List(Nil)),
      PartialMember("name", scexType[String])
    ))
  }
}

object ScopeAndTypeCompletionTest {

  trait Root {
    @ParameterNames(Array("annotArg"))
    @Documentation("handles stuff")
    def method(arg: Any, moar: Any): Any
  }

  trait SvRoot {
    def sv: DynStr
  }

  trait DynStr extends Dynamic {
    def selectDynamic(str: String): String
  }

}
