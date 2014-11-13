package com.avsystem.scex.compiler

import java.io.File
import java.{lang => jl, util => ju}

import com.google.common.cache.CacheBuilder

import scala.collection.mutable
import scala.reflect.io.{AbstractFile, Directory, PlainDirectory}
import scala.tools.nsc.Phase
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.util.ClassPath

/**
 * An adaptation of ScexCompiler which compiles non-shared classes to disk instead of memory (assuming that classfile
 * directory is configured). Class files are never being deleted automatically and thus are reused even if the entire
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
    if (!ClassPath.split(settings.classpath.value).contains(classfileDir.path)) {
      settings.classpath.append(classfileDir.path)
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

  override protected def createNonSharedClassLoader(sourceFile: ScexSourceFile) =
    stateOpt.map { state =>
      import state._

      classfileDir.file.mkdirs()
      val sourceName = sourceFile.file.name

      def createClassLoader =
        new ScexClassLoader(classfileDir.subdirectoryNamed(sourceName), getSharedClassLoader)

      nonSharedClassLoaders.get(sourceName, callable(createClassLoader))
    } getOrElse super.createNonSharedClassLoader(sourceFile)

  override protected def setup(): Unit = {
    _stateOpt = Option(settings.classfileDirectory.value)
      .map(_.trim).filter(_.nonEmpty).map(path => new State(new PlainDirectory(new Directory(new File(path)))))
    super.setup()
  }

  override protected def runCompiler(global: ScexGlobal, sourceFile: ScexSourceFile): Unit = {
    import global._
    new Run

    def isValid(signature: String): Boolean =
      signature.split("\\n{2,}").iterator.map(_.trim).filter(!_.isEmpty).forall { sig =>
        val Array(typedSig, erasedSig) = sig.split("\n")
        val Array(fullName, _) = typedSig.split(":")

        def symbolsWithName(owner: Symbol, nameParts: List[String]): Iterator[Symbol] =
          nameParts match {
            case namePart :: rest if owner.isClass || owner.isModule =>
              val ownerType = owner.toType
              val members = Iterator(ownerType.member(TypeName(namePart))) ++ alternatives(ownerType.member(TermName(namePart)))
              members.filter(_ != NoSymbol).flatMap(symbolsWithName(_, rest))
            case Nil => Iterator(owner)
            case _ => Iterator.empty
          }

        symbolsWithName(RootClass, fullName.split("\\.").toList)
          .filter(_.isTerm).flatMap(s => s :: s.overrides)
          .map(s => (typedSignature(global)(s.asTerm), erasedSignature(global)(s.asTerm)))
          .contains((typedSig, erasedSig))
      }

    val sigFileName = sourceFile.file.name + ".sig"
    val optimizedRun = for {
      state <- stateOpt
      outDir <- global.settings.outputDirs.getSingleOutput
      sigFile <- Option(outDir.lookupName(sigFileName, directory = false)) if isValid(new String(sigFile.toCharArray))
    } yield {
      logger.debug(s"Expression source file ${sourceFile.file.name} has already been compiled and bytecode is compatible.")
    }
    optimizedRun getOrElse super.runCompiler(global, sourceFile)
  }

  private def erasedSignature(global: ScexGlobal)(sym: global.TermSymbol) = try {
    import global._
    if (sym.isClassConstructor) {
      val constr = constructorToJava(sym.asMethod)
      val paramsSignature: String = constr.getParameterTypes.map(_.getName).mkString("(", ",", ")")
      constr.getDeclaringClass.getName + paramsSignature
    } else if (sym.isMethod) {
      val meth = methodToJava(sym.asMethod)
      val paramsSignature: String = meth.getParameterTypes.map(_.getName).mkString("(", ",", ")")
      meth.getDeclaringClass.getName + "." + meth.getName + paramsSignature + meth.getReturnType.getName
    } else if (isJavaField(sym)) {
      val fld = fieldToJava(sym)
      fld.getDeclaringClass.getName + "." + fld.getName + ":" + fld.getType.getName
    } else if (sym.isModule) {
      typeToJavaClass(sym.toType).getName
    } else "<none>"
  } catch {
    case _: NoSuchMethodException | _: NoSuchFieldException | _: ClassNotFoundException =>
      "<none>"
  }

  private def typedSignature(global: ScexGlobal)(sym: global.TermSymbol) =
    sym.fullName + ":" + sym.info.paramLists.map(_.map(_.typeSignature.toString()).mkString("(", ",", ")")).mkString +
      sym.typeSignature.finalResultType.toString()

  private class SignatureGenerator(val global: ScexGlobal) extends Plugin {
    plugin =>

    import global._

    val name = "signatureGenerator"
    val components: List[PluginComponent] = List(genSignature, saveSignature)
    val description = "SCEX signature generator"

    private val sigs = new mutable.WeakHashMap[CompilationUnit, String]

    private abstract class BaseComponent(runsAfterPhase: String, val phaseName: String) extends PluginComponent {
      val global: plugin.global.type = plugin.global
      val runsAfter = List(runsAfterPhase)

      def newPhase(prev: Phase) = new StdPhase(prev) {
        override def apply(unit: CompilationUnit): Unit =
          applyComponentPhase(unit)
      }

      def applyComponentPhase(unit: CompilationUnit): Unit
    }

    private object genSignature extends BaseComponent("typer", "genSignature") {
      def applyComponentPhase(unit: CompilationUnit): Unit = for {
        state <- stateOpt
        outDir <- global.settings.outputDirs.getSingleOutput
      } {
        unit.body.find(_.hasAttachment[ExpressionTreeAttachment.type]).foreach { tree =>
          val signatures = new mutable.HashSet[String]

          tree.foreach { t =>
            val s = t.symbol
            if (s != null && s.isTerm && !s.hasPackageFlag && s.sourceFile != unit.source.file) {
              signatures += typedSignature(global)(s.asTerm) + "\n" + erasedSignature(global)(s.asTerm)
            }
          }

          sigs(unit) = signatures.toList.sorted.mkString("\n\n")
        }
      }
    }

    // signature is saved after full compilation when we're sure that there were no compilation errors
    private object saveSignature extends BaseComponent("jvm", "saveSignature") {
      def applyComponentPhase(unit: CompilationUnit): Unit = for {
        state <- stateOpt
        outDir <- global.settings.outputDirs.getSingleOutput
        sig <- sigs.get(unit)
      } {
        logger.debug(s"Saving signatures file for ${unit.source.file.name}:\n$sig")
        val os = outDir.fileNamed(unit.source.file.name + ".sig").output
        try os.write(sig.getBytes) finally os.close()
        sigs.remove(unit)
      }
    }

  }

  override protected def loadCompilerPlugins(global: ScexGlobal) =
    new SignatureGenerator(global) :: super.loadCompilerPlugins(global)
}
