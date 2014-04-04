package com.avsystem.scex
package compiler

import com.avsystem.scex.compiler.ScexCompiler.CompilationFailedException
import com.avsystem.scex.japi.DefaultJavaScexCompiler
import com.avsystem.scex.validation.SymbolValidator.MemberAccessSpec
import com.avsystem.scex.validation.{SymbolValidator, SyntaxValidator}
import com.avsystem.scex.{PredefinedAccessSpecs, ExpressionProfile}
import java.{util => ju, lang => jl}
import scala.reflect.runtime.universe.TypeTag
import com.avsystem.scex.util.SimpleContext

/**
 * Created: 18-11-2013
 * Author: ghik
 */
trait CompilationTest {
  val compiler = new DefaultJavaScexCompiler(new ScexCompilerConfig)

  def catchAndPrint(code: => Any) {
    try code catch {
      case t: Throwable => t.printStackTrace(System.out)
    }
  }

  def createProfile(acl: List[MemberAccessSpec], header: String = "import com.avsystem.scex.compiler._", utils: String = "") =
    new ExpressionProfile(SyntaxValidator.SimpleExpressions, SymbolValidator(acl), header, utils)

  def assertMemberAccessForbidden(expr: => Any) {
    try expr catch {
      case e: CompilationFailedException =>
        assert(e.errors.forall(_.msg.startsWith("Member access forbidden")))
    }
  }

  def evaluateTemplate[T: TypeTag](expr: String, acl: List[MemberAccessSpec] = defaultAcl, header: String = "") =
    compiler.getCompiledExpression[SimpleContext[Unit], T](
      createProfile(acl), expr, template = true, header).apply(SimpleContext(()))

  def evaluate[T: TypeTag](expr: String, acl: List[MemberAccessSpec] = defaultAcl) = {
    compiler.getCompiledExpression[SimpleContext[Unit], T](createProfile(acl), expr, template = false).apply(SimpleContext(()))
  }

  def defaultAcl = PredefinedAccessSpecs.basicOperations
}
