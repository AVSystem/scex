package com.avsystem.scex.compiler

import java.io.File
import java.{lang => jl, util => ju}

import com.google.common.cache.CacheBuilder

import scala.reflect.internal.util.SourceFile
import scala.reflect.io.{AbstractFile, Directory, PlainDirectory}

/**
 * An adaptation of ScexCompiler which compiles classes to disk instead of memory (assuming that classfile directory
 * is configured). Class files are never being deleted automatically and thus are reused even if the entire
 * process is restarted.
 *
 * Created: 21-10-2014
 * Author: ghik
 */
trait ClassfilePersistingScexCompiler extends ScexCompiler {

  import com.avsystem.scex.util.CommonUtils._

  private val logger = createLogger[ClassfilePersistingScexCompiler]

  private class State(val classfileDir: AbstractFile) {
    val sharedDir = classfileDir.subdirectoryNamed("shared")
    val nonSharedDir = classfileDir.subdirectoryNamed("non-shared")

    val sharedClassLoader = new ScexClassLoader(sharedDir, getClass.getClassLoader)
    settings.classpath.append(sharedDir.path)

    val nonSharedClassLoaders = CacheBuilder.newBuilder.weakValues.build[String, ScexClassLoader]
  }

  private var _stateOpt: Option[State] = null

  private def stateOpt = {
    if (_stateOpt == null) {
      init()
    }
    _stateOpt
  }

  override protected def compile(sourceFile: SourceFile, shared: Boolean) = stateOpt match {
    case Some(state) =>
      import state._

      sharedDir.file.mkdirs()
      nonSharedDir.file.mkdirs()

      val sourceName = sourceFile.file.name
      val targetDir = if (shared) sharedDir else nonSharedDir.subdirectoryNamed(sourceName)

      val errors = if (targetDir.isEmpty || shared) {
        compileTo(sourceFile, targetDir)
      } else {
        logger.debug(s"File $sourceFile has already been compiled to $targetDir")
        Nil
      }

      if (errors.isEmpty) {
        val classLoader =
          if (shared) sharedClassLoader
          else nonSharedClassLoaders.get(sourceName, callable(new ScexClassLoader(targetDir, sharedClassLoader)))

        Left(classLoader)
      } else Right(errors)

    case None =>
      super.compile(sourceFile, shared)
  }

  override def reset() = underLock {
    super.reset()
    init()
  }

  private def init(): Unit = {
    _stateOpt = Option(settings.classfileDirectory.value)
      .map(_.trim).filter(_.nonEmpty).map(path => new State(new PlainDirectory(new Directory(new File(path)))))
  }
}
