package com.avsystem.scex.compiler

import java.{lang => jl, util => ju}

import com.avsystem.scex.ExpressionProfile
import com.avsystem.scex.compiler.presentation.ScexPresentationCompiler
import com.avsystem.scex.util.SimpleContext
import com.avsystem.scex.validation.{SymbolValidator, SyntaxValidator}
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

  val emptyProfile = new ExpressionProfile("test", SyntaxValidator.SimpleExpressions, SymbolValidator(Nil), "", "")

  def applyIntExpr(expr: String) =
    compiler.getCompiledExpression[SimpleContext[Unit], Int](emptyProfile, expr, template = false).apply(SimpleContext(()))

  def compiledSourceNames = compiler.sourcesCompiled.map(_.file.name)

  before {
    val classfileDir = AbstractFile.getDirectory(compiler.settings.classfileDirectory.value)
    if (classfileDir != null) {
      classfileDir.delete()
    }
    compiler.reset()
  }

  test("non-shared classfile reusing test") {
    val c1 = compiler.compileClass("class Stuff", "Stuff")
    assert(compiler.sourcesCompiled.size === 1)

    val c2 = compiler.compileClass("class Stuff", "Stuff")
    assert(compiler.sourcesCompiled.size === 1)

    assert(c1 === c2) // both compilations should use the same class loader

    compiler.reset()

    val c3 = compiler.compileClass("class Stuff", "Stuff")
    assert(compiler.sourcesCompiled.size === 0)
    assert(c1 !== c3) // there will be new class loader after reset
  }

  test("shared classfile reusing test") {
    val c1 = compiler.compileSymbolValidator("test", "Nil").getClass
    assert(compiler.sourcesCompiled.size === 1)

    val c2 = compiler.compileSymbolValidator("test", "Nil").getClass
    assert(compiler.sourcesCompiled.size === 1)

    assert(c1 === c2) // both compilations should use the same class loader

    compiler.reset()

    val c3 = compiler.compileSymbolValidator("test", "Nil").getClass
    assert(compiler.sourcesCompiled.size === 0)
    assert(c1 !== c3) // there will be new class loader after reset
  }

  test("reusing profile classfiles test") {
    assert(applyIntExpr("1") === 1)
    assert(compiledSourceNames.contains("_scex_profile_test"))

    compiler.reset()

    assert(applyIntExpr("2") === 2)
    assert(!compiledSourceNames.contains("_scex_profile_test"))
  }

  test("visibility from presentation compiler test") {
    val completer = compiler.getCompleter[SimpleContext[Unit], Int](emptyProfile, template = false)

    assert(completer.getErrors("1") === Nil)
    assert(compiledSourceNames.contains("_scex_profile_test"))

    compiler.reset()

    assert(completer.getErrors("2") === Nil)
    assert(!compiledSourceNames.contains("_scex_profile_test"))
  }

  test("recompilation on binary compatibility breach test") {
    import SymbolValidator._

    val acl = allow {
      Predef.implicitly(_: Any)
    }
    val symbolValidator = SymbolValidator(acl)
    val expr = "implicitly[String]"

    val utils1 =
      """
        |implicit def implicitString1: String = "implicitString1"
      """.stripMargin
    val profile1 = new ExpressionProfile("test", SyntaxValidator.SimpleExpressions, symbolValidator, "", utils1)
    val cexpr1 = compiler.getCompiledExpression[SimpleContext[Unit], String](profile1, expr, template = false)

    assert(cexpr1(SimpleContext(())) === "implicitString1")

    compiler.reset()

    val utils2 =
      """
        |implicit def implicitString2: String = "implicitString2"
      """.stripMargin
    val profile2 = new ExpressionProfile("test", SyntaxValidator.SimpleExpressions, symbolValidator, "", utils2)
    val cexpr2 = compiler.getCompiledExpression[SimpleContext[Unit], String](profile2, expr, template = false)

    assert(cexpr2(SimpleContext(())) === "implicitString2")
  }

}
