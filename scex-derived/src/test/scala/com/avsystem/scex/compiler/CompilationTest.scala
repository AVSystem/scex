package com.avsystem.scex.compiler

import java.{util => ju, lang => jl}
import scala.collection.JavaConverters._
import scala.reflect.{ClassTag, classTag}
import com.avsystem.scex.japi.DefaultJavaScexCompiler
import com.avsystem.scex.validation.SymbolValidator.MemberAccessSpec
import com.avsystem.scex.ExpressionProfile
import com.avsystem.scex.validation.{SymbolValidator, SyntaxValidator}
import com.avsystem.scex.compiler.ScexCompiler.CompilationFailedException

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
}
