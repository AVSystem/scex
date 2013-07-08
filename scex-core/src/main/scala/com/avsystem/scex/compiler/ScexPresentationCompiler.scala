package com.avsystem.scex.compiler

import com.avsystem.scex.validation.ExpressionValidator
import java.{util => ju, lang => jl}
import scala.reflect.internal.util.{SourceFile, BatchSourceFile}
import scala.tools.nsc.interactive

trait ScexPresentationCompiler extends ScexCompiler {
  compiler =>

  private val reporter = new Reporter(settings)
  private var global: interactive.Global = _

  private def init() {
    global = new interactive.Global(settings, reporter)
  }

  init()

  private def newInteractiveExpressionPackage() =
    newPackageName("_scex_interactive_expr")

  class InteractiveContext(
    profile: ExpressionProfile,
    contextType: String,
    contextClass: Class[_],
    resultType: String) {

    val pkgName = newInteractiveExpressionPackage()

    def getErrors(expression: String) = compiler.synchronized {
      val iglobal = compiler.global

      def getOrThrow[T](resp: iglobal.Response[T]) = resp.get match {
        case Left(res) => res
        case Right(t) => throw t
      }

      getOrThrow(iglobal.askForResponse(() => ExpressionValidator.profileVar.value = profile))

      val exprDef = ExpressionDef(profile, expression, contextClass, contextType, resultType)
      val response = new iglobal.Response[iglobal.Tree]
      iglobal.askLoadedTyped(new BatchSourceFile(pkgName, expressionCode(exprDef, pkgName)), response)
      getOrThrow(response)

      getOrThrow(iglobal.askForResponse(() => ExpressionValidator.profileVar.value = null))

      reporter.compileErrors()
    }
  }

  def interactiveContext(profile: ExpressionProfile,
    contextType: String,
    contextClass: Class[_],
    resultType: String): InteractiveContext = {

    require(profile != null, "Profile cannot be null")
    require(contextType != null, "Context type cannot be null")
    require(contextClass != null, "Context class cannot be null")
    require(resultType != null, "Result type cannot be null")

    new InteractiveContext(profile, contextType, contextClass, resultType)
  }


  override protected def compile(sourceFile: SourceFile, classLoader: ScexClassLoader, shared: Boolean) = {
    if (shared) {
      val global = this.global
      global.askReload(List(sourceFile), new global.Response[Unit])
    }
    super.compile(sourceFile, classLoader, shared)
  }

  override def reset() {
    synchronized {
      super.reset()
      init()
    }
  }
}
