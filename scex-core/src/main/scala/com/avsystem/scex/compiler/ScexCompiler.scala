package com.avsystem.scex
package compiler

import java.{lang => jl, util => ju}

import com.avsystem.scex.compiler.CodeGeneration._
import com.avsystem.scex.compiler.ScexCompiler._
import com.avsystem.scex.util.CommonUtils._
import com.avsystem.scex.util.LoggingUtils
import com.avsystem.scex.validation.{SymbolValidator, SyntaxValidator}
import org.apache.commons.codec.digest.DigestUtils

import scala.collection.mutable.ListBuffer
import scala.reflect.NameTransformer
import scala.reflect.internal.util._
import scala.reflect.io.{AbstractFile, VirtualDirectory}
import scala.reflect.runtime.universe.TypeTag
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.reporters.AbstractReporter
import scala.tools.nsc.{Global, Settings}
import scala.util.{Success, Try}

trait ScexCompiler extends LoggingUtils {

  private val logger = createLogger[ScexCompiler]

  private object lock

  class Reporter(val settings: Settings) extends AbstractReporter {
    private val errorsBuilder = new ListBuffer[CompileError]

    def compileErrors() = {
      errorsBuilder.result()
    }

    def includes(pos1: Position, pos2: Position) =
      pos1.start <= pos2.start && pos1.end > pos2.end

    // standard `lineContent` method fails on last line (wtf?)
    def lineContent(pos: Position) = if (pos.source ne NoSourceFile) {
      val start = pos.source.lineToOffset(pos.line - 1)
      var end = start
      while (!pos.source.isEndOfLine(end) && end < pos.source.length) end += 1
      new String(pos.source.content, start, end - start)
    } else ""

    def display(pos: Position, msg: String, severity: Severity) {
      if (severity == ERROR) {
        val actualPos = pos.source match {
          case source: ExpressionSourceFile if includes(source.expressionPos, pos) =>
            pos.withSource(source.bareSource).withShift(-source.expressionPos.start)
          case _ => pos
        }
        errorsBuilder += CompileError(lineContent(actualPos), if (actualPos.isDefined) actualPos.column else 1, msg)
      }
    }

    def displayPrompt(): Unit = {}

    override def reset(): Unit = {
      super.reset()
      errorsBuilder.clear()
    }
  }

  protected class ScexClassLoader(val classfileDirectory: AbstractFile, parent: ClassLoader)
    extends AbstractFileClassLoader(classfileDirectory, parent) {

    // locking on classfile directory which otherwise could be modified by compiler during class loading
    override def getClassLoadingLock(className: String) = classfileDirectory
  }

  protected type RawExpression = Expression[ExpressionContext[_, _], Any]

  protected def underLock[T](code: => T) = lock.synchronized {
    ensureSetup()
    code
  }

  val settings: ScexSettings
  private lazy val reporter = new Reporter(settings)

  private var initialized = false
  private var global: ScexGlobal = _

  /**
   * Classloader for stuff that will be never reclaimed after compilation -
   * profiles, validators, custom util classes, etc.
   */
  private var sharedClassLoader: ScexClassLoader = _

  private var compilationCount: Int = _

  protected def setup(): Unit = {
    logger.info("Initializing Scala compiler")
    compilationCount = 0
    global = new Global(settings, reporter) with ScexGlobal {
      override def loadAdditionalPlugins() = loadCompilerPlugins(this)
    }
    sharedClassLoader = new ScexClassLoader(new VirtualDirectory("(scex_shared)", None), getClass.getClassLoader)
  }

  protected final def ensureSetup(): Unit = lock.synchronized {
    if (!initialized) {
      setup()
      initialized = true
    }
  }

  protected def loadCompilerPlugins(global: ScexGlobal): List[Plugin] = Nil

  private def instantiate[T](classLoader: ClassLoader, className: String) =
    Class.forName(className, true, classLoader).newInstance.asInstanceOf[T]

  protected def compileJavaGetterAdapter(clazz: Class[_], full: Boolean): Try[Option[String]] =
    generateJavaGetterAdapter(clazz, full) match {
      case Some(code) => underLock {
        val codeToCompile = wrapInSource(code, AdaptersPkg)
        val name = adapterName(clazz, full)
        val sourceFile = new ScexSourceFile(name, codeToCompile, shared = true)

        def result() = {
          compile(sourceFile) match {
            case Left(_) => Some(name)
            case Right(errors) => throw new CompilationFailedException(codeToCompile, errors)
          }
        }

        Try(result())
      }
      case None => Success(None)
    }

  protected def compileProfileObject(profile: ExpressionProfile): Try[String] = {
    val adapters = profile.symbolValidator.referencedJavaClasses.toVector.sortBy(_.getName).flatMap {
      clazz => compileJavaGetterAdapter(clazz, full = false).get.map(adapterName => (clazz, adapterName))
    }

    underLock {
      val pkgName = ProfilePkgPrefix + NameTransformer.encode(profile.name)
      val codeToCompile = wrapInSource(generateProfileObject(profile, adapters), pkgName)
      val sourceFile = new ScexSourceFile(pkgName, codeToCompile, shared = true)

      def result =
        compile(sourceFile) match {
          case Left(_) => pkgName
          case Right(errors) => throw new CompilationFailedException(codeToCompile, errors)
        }

      Try(result)
    }
  }

  protected def compileExpressionUtils(utils: NamedSource): Try[String] = underLock {
    val pkgName = UtilsPkgPrefix + NameTransformer.encode(utils.name)
    val codeToCompile = wrapInSource(generateExpressionUtils(utils.code), pkgName)
    val sourceFile = new ScexSourceFile(pkgName, codeToCompile, shared = true)

    def result =
      compile(sourceFile) match {
        case Left(_) => pkgName
        case Right(errors) => throw new CompilationFailedException(codeToCompile, errors)
      }

    Try(result)
  }

  protected final def expressionCode(exprDef: ExpressionDef, noMacroProcessing: Boolean = false): (String, String, Int) = {
    val profile = exprDef.profile
    val rootObjectClass = exprDef.rootObjectClass

    val fullAdapterClassNameOpt =
      if (profile.symbolValidator.referencedJavaClasses.contains(rootObjectClass)) {
        compileJavaGetterAdapter(rootObjectClass, full = true).get.map(name => s"$AdaptersPkg.$name")
      } else None

    val profileObjectPkg = compileProfileObject(profile).get
    val utilsObjectPkg = compileExpressionUtils(profile.expressionUtils).get
    val (expressionCode, offset) =
      generateExpressionClass(exprDef, fullAdapterClassNameOpt, profileObjectPkg, utilsObjectPkg, noMacroProcessing)
    val pkgName = ExpressionPkgPrefix + DigestUtils.md5Hex(expressionCode)

    wrapInSource(expressionCode, offset, pkgName)
  }

  protected def getSharedClassLoader: ScexClassLoader =
    sharedClassLoader

  protected def createNonSharedClassLoader(sourceFile: ScexSourceFile): ScexClassLoader =
    new ScexClassLoader(new VirtualDirectory(sourceFile.file.name, None), getSharedClassLoader)

  protected def compile(sourceFile: ScexSourceFile): Either[ScexClassLoader, Seq[CompileError]] = {
    compilationCount += 1

    val classLoader = if (sourceFile.shared) getSharedClassLoader else createNonSharedClassLoader(sourceFile)
    val classfileDirectory = classLoader.classfileDirectory

    settings.outputDirs.setSingleOutput(classfileDirectory)
    reporter.reset()

    logger.debug(s"Compiling source file ${sourceFile.path} to $classfileDirectory:\n${new String(sourceFile.content)}")

    val startTime = System.nanoTime

    // ScexClassLoader loads classes while being locked on classfileDirectory.
    // Compiler writes classes to this directory, so synchronization over it is needed during compilation.
    // So, compilation is effectively under two locks: ScexCompiler's internal lock and classfileDirectory, in that order.
    // There should not be deadlocks, because nobody locks first over classfileDirectory and then over ScexCompiler.
    classfileDirectory.synchronized {
      val global = this.global
      val run = new global.Run
      run.compileSources(List(sourceFile))
      if (!sourceFile.shared) {
        global.forgetSymbolsFromSource(sourceFile.file)
      }
    }

    val duration = System.nanoTime - startTime
    logger.debug(s"Compilation took ${duration / 1000000}ms")

    val errors = reporter.compileErrors()

    if (compilationCount > settings.resetAfterCount.value) {
      reset()
    }

    if (errors.isEmpty) Left(classLoader) else Right(errors)
  }

  protected def preprocess(exprDef: ExpressionDef): ExpressionDef =
    exprDef

  protected def compileExpression(exprDef: ExpressionDef): Try[RawExpression] = underLock {
    val preprocessedExprDef = preprocess(exprDef)
    val (pkgName, codeToCompile, offset) = expressionCode(preprocessedExprDef)
    // every single expression has its own classloader and virtual directory
    val sourceFile = new ExpressionSourceFile(preprocessedExprDef, pkgName, codeToCompile, offset)
    val sourceInfo = new SourceInfo(pkgName, codeToCompile, offset, offset + exprDef.expression.length,
      sourceFile.offsetToLine(offset) + 1, sourceFile.offsetToLine(offset + exprDef.expression.length - 1) + 2)
    val debugInfo = new ExpressionDebugInfo(exprDef, sourceInfo)

    def result =
      compile(sourceFile) match {
        case Left(classLoader) =>
          Class.forName(s"$pkgName.$ExpressionClassName", true, classLoader)
            .getConstructor(classOf[ExpressionDebugInfo]).newInstance(debugInfo)
            .asInstanceOf[RawExpression]

        case Right(errors) =>
          throw new CompilationFailedException(codeToCompile, errors)
      }

    Try(result)
  }

  protected final def getCompiledExpression[C <: ExpressionContext[_, _], T](exprDef: ExpressionDef): Expression[C, T] =
    compileExpression(exprDef).get.asInstanceOf[Expression[C, T]]

  def getCompiledExpression[C <: ExpressionContext[_, _] : TypeTag, T: TypeTag](
    profile: ExpressionProfile,
    expression: String,
    template: Boolean = true,
    header: String = ""): Expression[C, T] = {

    require(profile != null, "Profile cannot be null")
    require(expression != null, "Expression cannot be null")
    require(header != null, "Header cannot be null")

    import scala.reflect.runtime.universe._

    val mirror = typeTag[C].mirror
    val contextType = typeOf[C]
    val TypeRef(_, _, List(rootObjectType, _)) = contextType.baseType(typeOf[ExpressionContext[_, _]].typeSymbol)
    val rootObjectClass =
      try mirror.runtimeClass(rootObjectType) catch {
        case _: ClassNotFoundException => null
      }

    getCompiledExpression(ExpressionDef(profile, template, setter = false, expression, PositionMapping.empty,
      header, rootObjectClass, contextType.toString, typeOf[T].toString))
  }

  def getCompiledSetterExpression[C <: ExpressionContext[_, _] : TypeTag, T: TypeTag](
    profile: ExpressionProfile,
    expression: String,
    template: Boolean = true,
    header: String = ""): Expression[C, Setter[T]] = {

    require(profile != null, "Profile cannot be null")
    require(expression != null, "Expression cannot be null")
    require(header != null, "Header cannot be null")

    import scala.reflect.runtime.universe._

    val mirror = typeTag[C].mirror
    val contextType = typeOf[C]
    val TypeRef(_, _, List(rootObjectType, _)) = contextType.baseType(typeOf[ExpressionContext[_, _]].typeSymbol)
    val rootObjectClass =
      try mirror.runtimeClass(rootObjectType) catch {
        case _: ClassNotFoundException => null
      }

    getCompiledExpression(ExpressionDef(profile, template, setter = true, expression, PositionMapping.empty,
      header, rootObjectClass, contextType.toString, typeOf[T].toString))
  }


  @throws[CompilationFailedException]
  def compileSyntaxValidator(source: NamedSource): SyntaxValidator = underLock {
    val pkgName = SyntaxValidatorPkgPrefix + NameTransformer.encode(source.name)
    val codeToCompile = wrapInSource(generateSyntaxValidator(source.code), pkgName)
    val sourceFile = new ScexSourceFile(pkgName, codeToCompile, shared = false)

    compile(sourceFile) match {
      case Left(classLoader) =>
        instantiate[SyntaxValidator](classLoader, s"$pkgName.$SyntaxValidatorClassName")
      case Right(errors) =>
        throw new CompilationFailedException(codeToCompile, errors)
    }
  }

  @throws[CompilationFailedException]
  def compileSymbolValidator(source: NamedSource): SymbolValidator = underLock {
    val pkgName = SymbolValidatorPkgPrefix + NameTransformer.encode(source.name)
    val codeToCompile = wrapInSource(generateSymbolValidator(source.code), pkgName)
    val sourceFile = new ScexSourceFile(pkgName, codeToCompile, shared = false)

    compile(sourceFile) match {
      case Left(classLoader) =>
        instantiate[SymbolValidator](classLoader, s"$pkgName.$SymbolValidatorClassName")
      case Right(errors) =>
        throw new CompilationFailedException(codeToCompile, errors)
    }
  }

  /**
   * Compiles arbitrary Scala source file into a dedicated class loader and loads class with given fully qualified name
   * from that class loader.
   */
  def compileClass(code: String, name: String): Class[_] = underLock {
    val sourceName = ArbitraryClassSourceNamePrefix + DigestUtils.md5Hex(code)
    val sourceFile = new ScexSourceFile(sourceName, code, shared = false)

    compile(sourceFile) match {
      case Left(classLoader) =>
        Class.forName(name, true, classLoader)
      case Right(errors) =>
        throw new CompilationFailedException(code, errors)
    }
  }

  /**
   * Resets internal compiler state by creating completely new instance of Scala compiler and invalidating all
   * internal caches.
   */
  def reset(): Unit =
    underLock(setup())
}

object ScexCompiler {

  case class CompilationFailedException(source: String, errors: Seq[CompileError])
    extends RuntimeException(s"Compilation failed with ${pluralize(errors.size, "error")}:\n${errors.mkString("\n")}")

  case class CompileError(line: String, column: Int, msg: String) {
    override def toString = s"$msg:\n${line.stripLineEnd}\n${" " * (column - 1)}^"
  }

}
