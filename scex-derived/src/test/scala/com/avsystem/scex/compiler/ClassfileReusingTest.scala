package com.avsystem.scex.compiler

import java.{lang => jl, util => ju}

import com.avsystem.scex.compiler.presentation.ScexPresentationCompiler
import com.avsystem.scex.presentation.SymbolAttributes
import com.avsystem.scex.util.SimpleContext
import com.avsystem.scex.validation.{SymbolValidator, SyntaxValidator}
import com.avsystem.scex.{ExpressionProfile, NamedSource, PredefinedAccessSpecs}
import org.scalatest.BeforeAndAfter

import scala.collection.mutable.ArrayBuffer
import scala.reflect.internal.util.SourceFile
import scala.reflect.io.AbstractFile
import scala.tools.nsc.Global

/**
 * Created: 22-10-2014
 * Author: ghik
 */
class ClassfileReusingTest extends ScexFunSuite with BeforeAndAfter {

  trait ScexCompilerInterceptor extends InterceptingPluginScexCompiler {
    val sourcesCompiled = new ArrayBuffer[SourceFile]

    protected def runsAfter = List("jvm")

    protected def intercept(global: Global)(unit: global.CompilationUnit): Unit = {
      if (unit.body != global.EmptyTree) {
        sourcesCompiled += unit.source
      }
    }

    override def reset() = {
      super.reset()
      sourcesCompiled.clear()
    }
  }

  object compiler
    extends ScexCompiler
    with ScexCompilerInterceptor
    with ScexPresentationCompiler
    with ClassfileReusingScexCompiler {

    val settings = new ScexSettings
    settings.classfileDirectory.value = "testClassfileCache"
  }

  val testProfile = new ExpressionProfile("test", SyntaxValidator.SimpleExpressions,
    SymbolValidator(PredefinedAccessSpecs.basicOperations), SymbolAttributes(Nil), "", NamedSource("<empty>", ""))

  def applyIntExpr(expr: String) =
    compiler.getCompiledExpression[SimpleContext[Unit], Int](testProfile, expr, template = false).apply(SimpleContext(()))

  def compiledSourceNames = compiler.sourcesCompiled.map(_.file.name)

  before {
    val classfileDir = AbstractFile.getDirectory(compiler.settings.classfileDirectory.value)
    if (classfileDir != null) {
      classfileDir.delete()
    }
    compiler.reset()
  }

  test("expression classfile reusing test") {
    val expr = "1.toDouble + 4.5.toInt + \"shiet\".toFloat"
    compiler.getCompiledExpression[SimpleContext[Unit], Any](testProfile, expr, template = false)
    val sourcesCompiled = compiler.sourcesCompiled.size
    assert(sourcesCompiled > 0)

    compiler.reset()

    compiler.getCompiledExpression[SimpleContext[Unit], Any](testProfile, expr, template = false)
    assert(compiler.sourcesCompiled.size === sourcesCompiled - 1)
  }

  test("recompilation on binary compatibility breach test") {
    import com.avsystem.scex.validation.SymbolValidator._

    val acl = allow {
      Predef.implicitly(_: Any)
    }
    val symbolValidator = SymbolValidator(acl)
    val expr = "implicitly[String]"

    val utils1 =
      """
        |implicit def implicitString1: String = "implicitString1"
      """.stripMargin
    val profile1 = new ExpressionProfile("test", SyntaxValidator.SimpleExpressions, symbolValidator,
      SymbolAttributes(Nil), "", NamedSource("test", utils1))
    val cexpr1 = compiler.getCompiledExpression[SimpleContext[Unit], String](profile1, expr, template = false)

    assert(cexpr1(SimpleContext(())) === "implicitString1")

    compiler.reset()

    val utils2 =
      """
        |implicit def implicitString2: String = "implicitString2"
      """.stripMargin
    val profile2 = new ExpressionProfile("test", SyntaxValidator.SimpleExpressions, symbolValidator,
      SymbolAttributes(Nil), "", NamedSource("test", utils2))
    val cexpr2 = compiler.getCompiledExpression[SimpleContext[Unit], String](profile2, expr, template = false)

    assert(cexpr2(SimpleContext(())) === "implicitString2")
  }

  ignore("recompilation on overloaded method addition") {
    val symbolValidator = SymbolValidator(PredefinedAccessSpecs.basicOperations)
    val expr = "utilMethod(42)"

    val utils1 =
      """
        |def utilMethod(any: Any): Any = any
      """.stripMargin
    val profile1 = new ExpressionProfile("test", SyntaxValidator.SimpleExpressions, symbolValidator,
      SymbolAttributes(Nil), "", NamedSource("test", utils1))
    val cexpr1 = compiler.getCompiledExpression[SimpleContext[Unit], Any](profile1, expr, template = false)

    assert(cexpr1(SimpleContext(())) === 42)

    compiler.reset()

    val utils2 =
      """
        |def utilMethod(any: Any): Any = any
        |def utilMethod(int: Int): Any = int*2
      """.stripMargin
    val profile2 = new ExpressionProfile("test", SyntaxValidator.SimpleExpressions, symbolValidator,
      SymbolAttributes(Nil), "", NamedSource("test", utils2))
    val cexpr2 = compiler.getCompiledExpression[SimpleContext[Unit], Any](profile2, expr, template = false)

    assert(cexpr2(SimpleContext(())) === 84)

  }

}
