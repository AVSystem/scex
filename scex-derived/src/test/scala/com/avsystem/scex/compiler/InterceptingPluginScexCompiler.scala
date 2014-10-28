package com.avsystem.scex.compiler

import java.{lang => jl, util => ju}

import scala.tools.nsc._
import scala.tools.nsc.plugins.{Plugin, PluginComponent}

/**
 * Created: 27-10-2014
 * Author: ghik
 */
trait InterceptingPluginScexCompiler extends ScexCompiler {

  protected def runsAfter: List[String]

  protected def intercept(global: Global)(unit: global.CompilationUnit): Unit

  private class Interceptor(val global: ScexGlobal) extends Plugin {
    plugin =>

    import global._

    val name = "interceptingPlugin"
    val components: List[PluginComponent] = List(component)
    val description = "SCEX generic interceptor"

    private object component extends PluginComponent {
      val global: plugin.global.type = plugin.global
      val runsAfter = InterceptingPluginScexCompiler.this.runsAfter
      val phaseName = "scexIntercept"

      def newPhase(prev: Phase) = new StdPhase(prev) {
        override def apply(unit: CompilationUnit): Unit = {
          intercept(global)(unit)
        }
      }
    }

  }

  override protected def loadCompilerPlugins(global: ScexGlobal) =
    new Interceptor(global) :: super.loadCompilerPlugins(global)
}
