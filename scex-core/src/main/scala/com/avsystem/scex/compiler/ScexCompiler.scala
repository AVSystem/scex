package com.avsystem.scex
package compiler

import java.util.concurrent.locks.ReentrantLock
import com.avsystem.commons.misc.TypeString
import com.avsystem.scex.compiler.CodeGeneration._
import com.avsystem.scex.compiler.ScexCompiler._
import com.avsystem.scex.parsing.{EmptyPositionMapping, PositionMapping}
import com.avsystem.scex.util.CommonUtils._
import com.avsystem.scex.util.LoggingUtils
import com.avsystem.scex.validation.{SymbolValidator, SyntaxValidator}
import org.apache.commons.codec.digest.DigestUtils

import scala.annotation.nowarn
import scala.collection.mutable.ListBuffer
import scala.reflect.NameTransformer
import scala.reflect.internal.util._
import scala.reflect.io.{AbstractFile, VirtualDirectory}
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.reporters.FilteringReporter
import scala.tools.nsc.{Global, Settings}
import scala.util.{Failure, Success, Try}

trait ScexCompiler extends LoggingUtils {

  private val logger = createLogger[ScexCompiler]

  private val lock = new ReentrantLock

  @nowarn("msg=deprecated")
  class Reporter(val settings: Settings) extends FilteringReporter {
    private val errorsBuilder = new ListBuffer[CompileError]

    def compileErrors(): List[CompileError] =
      errorsBuilder.result()

    def includes(pos1: Position, pos2: Position): Boolean =
      pos1.start <= pos2.start && pos1.end > pos2.end

    // standard `lineContent` method fails on last line (wtf?)
    def lineContent(pos: Position): String = if (pos.source ne NoSourceFile) {
      val start = pos.source.lineToOffset(pos.line - 1)
      var end = start
      while (!pos.source.isEndOfLine(end) && end < pos.source.length) end += 1
      new String(pos.source.content, start, end - start)
    } else ""

    def mapPosition(pos: Position, mapping: PositionMapping): Position =
      if (pos.isRange) {
        val result = pos
          .withStart(mapping(pos.start))
          .withPoint(mapping(pos.point))
          .withEnd(mapping(pos.end))
        if (pos.isTransparent) result.makeTransparent else result
      }
      else if (pos.isOffset)
        pos.withPoint(mapping(pos.point))
      else pos

    override def doReport(pos: Position, msg: String, severity: Severity): Unit =
      if (severity == ERROR) {
        val actualPos = pos.source match {
          case source: ExpressionSourceFile if includes(source.expressionPos, pos) =>
            val positionMapping = source.exprDef.positionMapping.reverse
            mapPosition(pos.withShift(-source.expressionPos.start), positionMapping).withSource(source.bareSource)
          case _ => pos
        }
        errorsBuilder += CompileError(lineContent(actualPos), if (actualPos.isDefined) actualPos.column else 1, msg)
      }

    def displayPrompt(): Unit = {}

    override def reset(): Unit = {
      super.reset()
      errorsBuilder.clear()
    }
  }

  protected class ScexClassLoader(val classfileDirectory: AbstractFile, parent: ClassLoader)
    extends AbstractFileClassLoader(classfileDirectory, parent)

  protected type RawExpression = Expression[ExpressionContext[_, _], Any]

  protected def underLock[T](code: => T): T = {
    ensureSetup()
    try {
      lock.lock()
      code
    } finally {
      lock.unlock()
    }
  }

  val settings: ScexSettings
  protected def compilerSettings: Settings = settings

  @volatile private var initialized = false
  private var global: ScexGlobal = _
  private var reporter: Reporter = _

  /**
   * Classloader for stuff that will be never reclaimed after compilation -
   * profiles, validators, custom util classes, etc.
   */
  private var sharedClassLoader: ScexClassLoader = _
  private var compilationCount: Int = _

  protected def setup(): Unit = {
    logger.info("Initializing Scala compiler")
    compilationCount = 0
    reporter = new Reporter(compilerSettings)
    global = new Global(compilerSettings, reporter) with ScexGlobal {
      override def loadAdditionalPlugins(): List[Plugin] = loadCompilerPlugins(this)

      def classLoader: ScexClassLoader = getSharedClassLoader
    }
    sharedClassLoader = new ScexClassLoader(new VirtualDirectory("(scex_shared)", None), getClass.getClassLoader)
  }

  protected final def ensureSetup(): Unit = {
    if (!initialized) {
      try {
        lock.lock()
        if (!initialized) {
          setup()
          initialized = true
        }
      } finally {
        lock.unlock()
      }
    }
  }

  protected def loadCompilerPlugins(global: ScexGlobal): List[Plugin] = Nil

  protected def instantiate[T](classLoader: ClassLoader, className: String): T =
    Class.forName(className, true, classLoader).getConstructor().newInstance().asInstanceOf[T]

  protected def compileJavaGetterAdapters(profile: ExpressionProfile, name: String, classes: Seq[Class[_]], full: Boolean): Try[Seq[Option[String]]] =
    if (settings.noGetterAdapters.value) Success(classes.map(_ => None))
    else {
      val (fullCode, names) = classes.foldLeft(("", Vector.empty[Option[String]])) {
        case ((prevCode, prevNames), clazz) => generateJavaGetterAdapter(clazz, full) match {
          case Some(code) => (prevCode + "\n" + code, prevNames :+ Some(adapterName(clazz, full)))
          case None => (prevCode, prevNames :+ None)
        }
      }
      val codeToCompile = wrapInSource(fullCode, AdaptersPkgPrefix + NameTransformer.encode(profile.name))
      val sourceFile = new ScexSourceFile(name + profile.name, codeToCompile, shared = true)

      def result() = {
        compile(sourceFile) match {
          case Left(_) => names
          case Right(errors) => throw CompilationFailedException(codeToCompile, errors)
        }
      }

      Try(result())
    }

  protected def compileProfileObject(profile: ExpressionProfile): Try[Option[String]] = underLock {
    val classes = profile.symbolValidator.referencedJavaClasses.toVector.sortBy(_.getName)
    val adapterNames = compileJavaGetterAdapters(profile, "Adapters_", classes, full = false).get
    val adapters = (classes zip adapterNames).collect {
      case (clazz, Some(adapterName)) => (clazz, adapterName)
    }

    generateProfileObject(profile, adapters) match {
      case Some(profileObjCode) =>
        val pkgName = ProfilePkgPrefix + NameTransformer.encode(profile.name)
        val codeToCompile = wrapInSource(profileObjCode, pkgName)
        val sourceFile = new ScexSourceFile(pkgName, codeToCompile, shared = true)

        def result =
          compile(sourceFile) match {
            case Left(_) => pkgName
            case Right(errors) => throw CompilationFailedException(codeToCompile, errors)
          }

        Try(Some(result))

      case None =>
        Success(None)
    }
  }

  protected def compileExpressionUtils(utils: NamedSource): Try[Option[String]] =
    if (utils.code.isEmpty) Success(None) else underLock {
      val pkgName = UtilsPkgPrefix + NameTransformer.encode(utils.name)
      val codeToCompile = wrapInSource(generateExpressionUtils(utils.code), pkgName)
      val sourceFile = new ScexSourceFile(pkgName, codeToCompile, shared = true)

      def result =
        compile(sourceFile) match {
          case Left(_) => pkgName
          case Right(errors) => throw CompilationFailedException(codeToCompile, errors)
        }

      Try(Some(result))
    }

  protected final def expressionCode(exprDef: ExpressionDef, noMacroProcessing: Boolean = false): (String, String, Int) = {
    val profile = exprDef.profile
    val rootObjectClass = exprDef.rootObjectClass

    val fullAdapterClassNameOpt =
      if (profile.symbolValidator.referencedJavaClasses.contains(rootObjectClass)) {
        val name = adapterName(rootObjectClass, full = true)
        compileJavaGetterAdapters(profile, name, Seq(rootObjectClass), full = true)
          .get.head.map(name => s"$AdaptersPkgPrefix${NameTransformer.encode(profile.name)}.$name")
      } else None

    val profileObjectPkg = compileProfileObject(profile).get
    val utilsObjectPkg = compileExpressionUtils(profile.expressionUtils).get
    val (expressionCode, offset) =
      generateExpressionClass(exprDef, fullAdapterClassNameOpt, profileObjectPkg, utilsObjectPkg, noMacroProcessing)
    val pkgName = ExpressionPkgPrefix + DigestUtils.md5Hex(expressionCode)

    wrapInSource(expressionCode, offset, pkgName)
  }

  protected final def withGlobal[T](code: ScexGlobal => T): T = underLock {
    reporter.reset()
    val global = this.global
    val result = try code(global) finally {
      reporter.reset()
    }
    result
  }

  protected def getSharedClassLoader: ScexClassLoader =
    sharedClassLoader

  protected def createNonSharedClassLoader(sourceFile: ScexSourceFile): ScexClassLoader =
    new ScexClassLoader(new VirtualDirectory(sourceFile.file.name, None), getSharedClassLoader)

  protected def compile(sourceFile: ScexSourceFile): Either[ScexClassLoader, List[CompileError]] = {
    compilationCount += 1

    val classLoader = if (sourceFile.shared) getSharedClassLoader else createNonSharedClassLoader(sourceFile)
    val classfileDirectory = classLoader.classfileDirectory

    reporter.reset()

    logger.debug(s"Compiling source file ${sourceFile.path} to $classfileDirectory:\n${new String(sourceFile.content)}")

    val startTime = System.nanoTime

    // ScexClassLoader loads classes while being locked on itself.
    // Compiler writes classes to this directory, so synchronization over it is needed during compilation.
    // So, compilation is effectively under two locks: ScexCompiler's internal lock and ScexClassLoader, in that order.
    // There should not be deadlocks, because nobody locks first over ScexClassLoader and then over ScexCompiler.
    classLoader.synchronized {
      val global = this.global
      global.settings.outputDirs.setSingleOutput(classfileDirectory)
      runCompiler(global, sourceFile)
    }

    val duration = System.nanoTime - startTime
    logger.debug(s"Compilation took ${duration / 1000000}ms")

    val errors = reporter.compileErrors()

    if (compilationCount > settings.resetAfterCount.value) {
      reset()
    }

    if (errors.isEmpty) Left(classLoader) else Right(errors)
  }

  protected def runCompiler(global: ScexGlobal, sourceFile: ScexSourceFile): Unit = {
    val run = new global.Run
    run.compileSources(List(sourceFile))
    if (!sourceFile.shared) {
      global.forgetSymbolsFromSource(sourceFile.file)
    }
  }

  protected def preprocess(expression: String, template: Boolean): (String, PositionMapping) =
    (expression, EmptyPositionMapping)

  protected def compileExpression(exprDef: ExpressionDef): Try[RawExpression] = underLock {
    val (pkgName, codeToCompile, offset) = expressionCode(exprDef)
    // every single expression has its own classloader and virtual directory
    val sourceFile = new ExpressionSourceFile(exprDef, pkgName, codeToCompile, offset)
    val sourceInfo = new SourceInfo(pkgName, codeToCompile, offset, offset + exprDef.expression.length,
      sourceFile.offsetToLine(offset) + 1, sourceFile.offsetToLine(offset + exprDef.expression.length - 1) + 2)
    val debugInfo = new ExpressionDebugInfo(exprDef)

    compile(sourceFile) match {
      case Left(classLoader) =>
        val clazz = Class.forName(s"$pkgName.$ExpressionClassName", true, classLoader)
        clazz.getDeclaredClasses
        // force loading of inner classes
        val expr = clazz.getConstructor(classOf[ExpressionDebugInfo], classOf[SourceInfo])
          .newInstance(debugInfo, sourceInfo)
          .asInstanceOf[RawExpression]
        Success(expr)

      case Right(errors) =>
        Failure(CompilationFailedException(codeToCompile, errors))
    }
  }

  protected final def getCompiledExpression[C <: ExpressionContext[_, _], T](exprDef: ExpressionDef): Expression[C, T] =
    compileExpression(exprDef).get.asInstanceOf[Expression[C, T]]

  def getCompiledExpression[C <: ExpressionContext[_, _], T](
    profile: ExpressionProfile,
    expression: String,
    variableTypes: Map[String, TypeString[_]] = Map.empty,
    template: Boolean = true,
    header: String = ""
  )(implicit
    cti: ContextTypeInfo[C],
    tts: TypeString[T]
  ): Expression[C, T] = {

    require(profile != null, "Profile cannot be null")
    require(expression != null, "Expression cannot be null")
    require(header != null, "Header cannot be null")

    val strVariableTypes = variableTypes.iterator.map({ case (k, v) => (k, v.value) }).toMap
    val (actualExpression, positionMapping) = preprocess(expression, template)
    getCompiledExpression(ExpressionDef(profile, template, setter = false, actualExpression,
      header, cti.fullTypeString, tts.value, strVariableTypes)(expression, positionMapping, cti.rootObjectClass))
  }

  def getCompiledSetterExpression[C <: ExpressionContext[_, _], T](
    profile: ExpressionProfile,
    expression: String,
    template: Boolean = true,
    variableTypes: Map[String, TypeString[_]] = Map.empty,
    header: String = ""
  )(implicit
    cti: ContextTypeInfo[C],
    tts: TypeString[T]
  ): Expression[C, Setter[T]] = {

    require(profile != null, "Profile cannot be null")
    require(expression != null, "Expression cannot be null")
    require(header != null, "Header cannot be null")

    val strVariableTypes = variableTypes.iterator.map({ case (k, v) => (k, v.value) }).toMap

    val (actualExpression, positionMapping) = preprocess(expression, template)
    getCompiledExpression(ExpressionDef(profile, template, setter = true, actualExpression,
      header, cti.fullTypeString, tts.value, strVariableTypes)(expression, positionMapping, cti.rootObjectClass))
  }

  @throws[CompilationFailedException]
  def compileSyntaxValidator(source: NamedSource): SyntaxValidator = underLock {
    val pkgName = SyntaxValidatorPkgPrefix + NameTransformer.encode(source.name)
    val codeToCompile = wrapInSource(generateSyntaxValidator(source.code), pkgName)
    val sourceFile = new ScexSourceFile(pkgName, codeToCompile, shared = true)

    compile(sourceFile) match {
      case Left(classLoader) =>
        instantiate[SyntaxValidator](classLoader, s"$pkgName.$SyntaxValidatorClassName")
      case Right(errors) =>
        throw CompilationFailedException(codeToCompile, errors)
    }
  }

  @throws[CompilationFailedException]
  def compileSymbolValidator(source: NamedSource): SymbolValidator = underLock {
    val pkgName = SymbolValidatorPkgPrefix + NameTransformer.encode(source.name)
    val codeToCompile = wrapInSource(generateSymbolValidator(source.code), pkgName)
    val sourceFile = new ScexSourceFile(pkgName, codeToCompile, shared = true)

    compile(sourceFile) match {
      case Left(classLoader) =>
        instantiate[SymbolValidator](classLoader, s"$pkgName.$SymbolValidatorClassName")
      case Right(errors) =>
        throw CompilationFailedException(codeToCompile, errors)
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
        throw CompilationFailedException(code, errors)
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

  final case class CompilationFailedException(source: String, errors: List[CompileError])
    extends RuntimeException(s"Compilation failed with ${pluralize(errors.size, "error")}:\n${errors.mkString("\n")}")

  final case class CompileError(line: String, column: Int, msg: String) {
    override def toString = s"$msg:\n${line.stripLineEnd}\n${" " * (column - 1)}^"
  }
}
