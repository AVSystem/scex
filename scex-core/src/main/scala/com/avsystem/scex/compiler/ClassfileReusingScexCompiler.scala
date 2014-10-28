package com.avsystem.scex.compiler

import java.io.File
import java.{lang => jl, util => ju}

import com.google.common.cache.CacheBuilder
import org.apache.commons.codec.digest.DigestUtils

import scala.collection.mutable
import scala.reflect.internal.util.SourceFile
import scala.reflect.io.{AbstractFile, Directory, PlainDirectory}
import scala.tools.nsc.Phase
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.util.ClassPath

/**
 * An adaptation of ScexCompiler which compiles classes to disk instead of memory (assuming that classfile directory
 * is configured). Class files are never being deleted automatically and thus are reused even if the entire
 * process is restarted.
 *
 * The decision about need for recompilation is made based on fully typechecked tree of source file being compiled.
 * This is handled by a compiler plugin which halts compilation after typer phase if it detects no changes.
 * So essentially, this extension of ScexCompiler provides a very simple incremental compilation like strategy.
 *
 * Created: 21-10-2014
 * Author: ghik
 */
trait ClassfileReusingScexCompiler extends ScexCompiler {

  import com.avsystem.scex.util.CommonUtils._

  private val logger = createLogger[ClassfileReusingScexCompiler]

  private class State(val classfileDir: AbstractFile) {
    val sharedDir = classfileDir.subdirectoryNamed("shared")
    val nonSharedDir = classfileDir.subdirectoryNamed("non-shared")

    val sharedClassLoader = new ScexClassLoader(sharedDir, getClass.getClassLoader)
    if (!ClassPath.split(settings.classpath.value).contains(sharedDir.path)) {
      settings.classpath.append(sharedDir.path)
    }

    val nonSharedClassLoaders = CacheBuilder.newBuilder.weakValues.build[String, ScexClassLoader]
  }

  private var _stateOpt: Option[State] = null

  private def stateOpt = {
    if (_stateOpt == null) {
      setup()
    }
    _stateOpt
  }

  override protected def chooseClassLoader(sourceFile: ScexSourceFile) = stateOpt match {
    case Some(state) =>
      import state._

      sharedDir.file.mkdirs()
      nonSharedDir.file.mkdirs()

      val sourceName = sourceFile.file.name

      def createNonSharedClassLoader =
        new ScexClassLoader(nonSharedDir.subdirectoryNamed(sourceName), sharedClassLoader)

      if (sourceFile.shared) sharedClassLoader
      else nonSharedClassLoaders.get(sourceName, callable(createNonSharedClassLoader))

    case None =>
      super.chooseClassLoader(sourceFile)
  }

  override protected def setup(): Unit = {
    _stateOpt = Option(settings.classfileDirectory.value)
      .map(_.trim).filter(_.nonEmpty).map(path => new State(new PlainDirectory(new Directory(new File(path)))))
    super.setup()
  }

  private class DigestChecker(val global: ScexGlobal) extends Plugin {
    plugin =>

    import global._

    val name = "digestChecker"
    val components: List[PluginComponent] = List(digestCheckingComponent, digestSavingComponent)
    val description = "SCEX digest checker"

    private val digests = new mutable.WeakHashMap[CompilationUnit, String]

    private abstract class BaseComponent(runsAfterPhase: String, val phaseName: String) extends PluginComponent {
      val global: plugin.global.type = plugin.global
      val runsAfter = List(runsAfterPhase)

      def newPhase(prev: Phase) = new StdPhase(prev) {
        override def apply(unit: CompilationUnit): Unit =
          applyComponentPhase(unit)
      }

      def applyComponentPhase(unit: CompilationUnit): Unit
    }

    private object digestCheckingComponent extends BaseComponent("typer", "digestChecking") {
      def applyComponentPhase(unit: CompilationUnit): Unit =
        (stateOpt, global.settings.outputDirs.getSingleOutput, unit.source) match {
          case (Some(state), Some(outDir), scexSource: ScexSourceFile) if !scexSource.shared =>

            val sourceName = unit.source.file.name
            val hashFile = outDir.fileNamed(sourceName + ".hash")
            val prevHash = new String(hashFile.toByteArray)

            val treeRepr = new StringBuilder(show(unit.body)).append("\n")
            unit.body.foreach { t =>
              if (t.tpe != null) {
                treeRepr.append(t.tpe.erasure.toString()).append("\n")
              }
              if (t.symbol != null) {
                treeRepr.append(t.symbol.fullName).append("\n")
              }
            }
            val newHash = DigestUtils.md5Hex(treeRepr.toString())

            if (prevHash != newHash) {
              digests(unit) = newHash
            } else {
              unit.body = EmptyTree
              //forgetSymbolsFromSource(unit.source)
              logger.debug(s"Source file $sourceName has already been compiled to $outDir")
            }

          case _ =>
        }
    }

    private object digestSavingComponent extends BaseComponent("jvm", "digestSaving") {
      def applyComponentPhase(unit: CompilationUnit): Unit =
        (stateOpt, global.settings.outputDirs.getSingleOutput, digests.get(unit)) match {
          case (Some(state), Some(outDir), Some(digest)) =>
            val os = outDir.fileNamed(unit.source.file.name + ".hash").output
            try os.write(digest.getBytes) finally os.close()
            digests.remove(unit)
          case _ =>
        }
    }

  }

  override protected def loadCompilerPlugins(global: ScexGlobal) =
    new DigestChecker(global) :: super.loadCompilerPlugins(global)
}
