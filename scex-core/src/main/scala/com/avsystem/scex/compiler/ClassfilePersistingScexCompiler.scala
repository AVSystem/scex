package com.avsystem.scex.compiler

import java.io.File
import java.{lang => jl, util => ju}

import org.apache.commons.codec.digest.DigestUtils

import scala.reflect.internal.util.SourceFile
import scala.reflect.io.{Directory, PlainDirectory, AbstractFile}

/**
 * Created: 21-10-2014
 * Author: ghik
 */
trait ClassfilePersistingScexCompiler extends ScexCompiler {

  private val logger = createLogger[ClassfilePersistingScexCompiler]

  private class State(val classfileDir: AbstractFile) {
    val sharedDir = classfileDir.subdirectoryNamed("shared")
    val nonSharedDir = classfileDir.subdirectoryNamed("non-shared")

    val sharedClassLoader = new ScexClassLoader(sharedDir, getClass.getClassLoader)
    settings.classpath.append(sharedDir.path)
  }

  private lazy val stateOpt = Option(settings.classfileDirectory.value)
    .map(_.trim).filter(_.nonEmpty).map(path => new State(new PlainDirectory(new Directory(new File(path)))))

  override protected def compile(sourceFile: SourceFile, shared: Boolean) = underLock(stateOpt match {
    case Some(state) =>
      import state._

      val sourceName = sourceFile.file.name
      val targetDir = if (shared) sharedDir else nonSharedDir.subdirectoryNamed(sourceName)

      lazy val hashFile = targetDir.fileNamed(sourceName + ".hash")
      lazy val prevHash = new String(hashFile.toByteArray)
      lazy val contentsHash = DigestUtils.md5Hex(sourceFile.file.toByteArray)

      val errors = if (targetDir.isEmpty || shared && prevHash != contentsHash) {
        val errors = compileTo(sourceFile, targetDir)

        if (shared && errors.isEmpty) {
          val os = hashFile.output
          try os.write(contentsHash.getBytes) finally os.close()
        }

        errors
      } else {
        logger.debug(s"File $sourceFile has already been compiled to $targetDir")
        Nil
      }

      if (errors.isEmpty) {
        val classLoader = if (shared) sharedClassLoader else new ScexClassLoader(targetDir, sharedClassLoader)
        Left(classLoader)
      } else Right(errors)

    case None =>
      super.compile(sourceFile, shared)
  })
}
