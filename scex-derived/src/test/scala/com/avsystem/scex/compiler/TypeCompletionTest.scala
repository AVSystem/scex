package com.avsystem.scex
package compiler

import java.{lang => jl, util => ju}

import com.avsystem.scex.util.SimpleContext
import org.scalatest.FunSuite

/**
 * Created: 07-10-2014
 * Author: ghik
 */
class TypeCompletionTest extends FunSuite with CompilationTest {

  import com.avsystem.scex.compiler.TypeCompletionTest._
  import com.avsystem.scex.compiler.presentation.ScexPresentationCompiler._
  import com.avsystem.scex.validation.SymbolValidator._

  private val header =
    """
      |import com.avsystem.scex.compiler.TypeCompletionTest._
    """.stripMargin

  private val acl = allow {
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

  val stringType = Type("String", classOf[String])
  val intType = Type("Int", classOf[Int])

  private val apiCompletion = Vector(
    Member("a", Nil, stringType, iimplicit = false),
    Member("b", Nil, intType, iimplicit = false),
    Member("c", List(Nil), intType, iimplicit = false),
    Member("d", List(List(Param("a", intType))), intType, iimplicit = false),
    Member("i", Nil, stringType, iimplicit = false)
  )

  private def assertCompletion(expr: String, completion: Vector[Member], offset: Int = -1): Unit = {
    val completer = createCompleter(acl)
    val actualOffset = if (offset < 0) offset + expr.length else offset
    val members = completer.getTypeCompletion(expr, actualOffset).members.sortBy(_.name)

    assert(members === completion)
  }

  test("basic completion test") {
    assertCompletion("api.", apiCompletion)
  }

  test("dynamic completion test") {
    assertCompletion("dyn.whatevs.", apiCompletion)
  }
}

object TypeCompletionTest {

  trait Root {
    def api: Api

    def dyn: Dyn
  }

  trait Api {
    def a: String

    def b: Int

    def c(): Int

    def d(a: Int): Int
  }

  trait ExtApi {
    def i: String
  }

  import scala.language.dynamics

  trait Dyn extends Dynamic {
    def selectDynamic(name: String): Api
  }

  implicit def apiToExt(api: Api): ExtApi = ???

}