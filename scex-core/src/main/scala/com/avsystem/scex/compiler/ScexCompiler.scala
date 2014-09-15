package com.avsystem.scex
package compiler

import java.{lang => jl, util => ju}

import com.avsystem.scex.compiler.CodeGeneration._
import com.avsystem.scex.compiler.ScexCompiler._
import com.avsystem.scex.util.CommonUtils._
import com.avsystem.scex.util.LoggingUtils
import com.avsystem.scex.validation.{SymbolValidator, SyntaxValidator}

import scala.collection.mutable.ListBuffer
import scala.reflect.internal.util.{AbstractFileClassLoader, BatchSourceFile, Position, SourceFile}
import scala.reflect.io.VirtualDirectory
import scala.reflect.runtime.universe.TypeTag
import scala.tools.nsc.reporters.AbstractReporter
import scala.tools.nsc.{Global, Settings}
import scala.util.Try

trait ScexCompiler extends PackageGenerator with LoggingUtils {

  val config: ScexCompilerConfig

  private val logger = createLogger[ScexCompiler]

  class Reporter(val settings: Settings) extends AbstractReporter {
    private val errorsBuilder = new ListBuffer[CompileError]

    def compileErrors() = {
      errorsBuilder.result()
    }

    def display(pos: Position, msg: String, severity: Severity) {
      if (severity == ERROR) {
        val actualPos = pos.source match {
          case source: ExpressionSourceFile if pos.point >= source.expressionPos.start && pos.point < source.expressionPos.end =>
            pos.withSource(source.bareSource).withShift(-source.expressionPos.start)
          case _ => pos
        }
        errorsBuilder += CompileError(actualPos.lineContent, if (actualPos.isDefined) actualPos.column else 1, msg)
      }
    }

    def displayPrompt(): Unit = {}

    override def reset(): Unit = {
      super.reset()
      errorsBuilder.clear()
    }
  }

  protected class ScexClassLoader(val classfileDirectory: VirtualDirectory, parent: ClassLoader)
    extends AbstractFileClassLoader(classfileDirectory, parent)

  protected type RawExpression = Expression[ExpressionContext[_, _], Any]

  protected def underLock[T](code: => T) = synchronized {
    if (!initialized) {
      init()
    }
    code
  }

  val settings = new Settings
  settings.usejavacp.value = true
  settings.exposeEmptyPackage.value = true
  // preserving 2.10 behaviour of macro expansion in presentation compiler
  // https://github.com/scala/scala/commit/6e4c926b4a4c5e8dd350ae3a150490a794b139ca
  // TODO: maybe try to make it work with MacroExpand.Discard ?
  settings.Ymacroexpand.value = settings.MacroExpand.Normal

  private var initialized = false

  private val reporter = new Reporter(settings)

  private var global: ScexGlobal = _

  /**
   * Classloader for stuff that will be never reclaimed after compilation -
   * profiles, validators, custom util classes, etc.
   */
  private var persistentClassLoader: ScexClassLoader = _

  private var compilationCount: Int = _

  private def init(): Unit = {
    logger.info("Initializing Scala compiler")
    compilationCount = 0
    global = new Global(settings, reporter) with ScexGlobal
    persistentClassLoader = new ScexClassLoader(new VirtualDirectory("(scex_persistent)", None), getClass.getClassLoader)
    initialized = true
  }

  protected def createDedicatedClassLoader(dirName: String) = underLock {
    new ScexClassLoader(new VirtualDirectory(dirName, None), persistentClassLoader)
  }

  private def instantiatePersistent[T](className: String) =
    Class.forName(className, true, persistentClassLoader).newInstance.asInstanceOf[T]

  private def newExpressionPackage() =
    newPackageName("_scex_expr")

  private def newProfilePackage() =
    newPackageName("_scex_profile")

  private def newAdapterPackage() =
    newPackageName("_scex_adapter")

  private def newSyntaxValidatorPackage() =
    newPackageName("_scex_syntax_validator")

  private def newSymbolValidatorPackage() =
    newPackageName("_scex_symbol_validator")

  protected def compileFullJavaGetterAdapter(clazz: Class[_]): Try[String] = underLock {
    val pkgName = newAdapterPackage()
    val codeToCompile = wrapInSource(generateJavaGetterAdapter(clazz, full = true).get, pkgName)
    val sourceFile = new BatchSourceFile(pkgName, codeToCompile)

    def result = {
      compile(sourceFile, persistentClassLoader, usedInExpressions = true) match {
        case Nil => pkgName
        case errors => throw new CompilationFailedException(codeToCompile, errors)
      }
    }

    Try(result)
  }

  protected def compileProfileObject(profile: ExpressionProfile): Try[String] = underLock {
    val pkgName = newProfilePackage()
    val codeToCompile = wrapInSource(generateProfileObject(profile), pkgName)
    val sourceFile = new BatchSourceFile(pkgName, codeToCompile)

    def result =
      compile(sourceFile, persistentClassLoader, usedInExpressions = true) match {
        case Nil => pkgName
        case errors => throw new CompilationFailedException(codeToCompile, errors)
      }

    Try(result)
  }

  protected def expressionCode(exprDef: ExpressionDef, pkgName: String, noMacroProcessing: Boolean = false): (String, Int) = {
    val profile = exprDef.profile
    val rootObjectClass = exprDef.rootObjectClass

    val fullAdapterClassNameOpt =
      if (profile.symbolValidator.referencedJavaClasses.contains(rootObjectClass)) {
        val adapterPkg = compileFullJavaGetterAdapter(rootObjectClass).get
        val adapterClassName = adapterName(rootObjectClass)
        Some(s"$adapterPkg.$adapterClassName")
      } else None

    val profileObjectPkg = compileProfileObject(profile).get
    val (expressionCode, offset) =
      generateExpressionClass(exprDef, fullAdapterClassNameOpt, profileObjectPkg, noMacroProcessing)

    wrapInSource(expressionCode, offset, pkgName)
  }

  protected def compile(sourceFile: SourceFile, classLoader: ScexClassLoader, usedInExpressions: Boolean): Seq[CompileError] = {
    compilationCount += 1

    settings.outputDirs.setSingleOutput(classLoader.classfileDirectory)
    reporter.reset()

    logger.debug(s"Compiling source file ${sourceFile.path}:\n${new String(sourceFile.content)}")

    val startTime = System.nanoTime

    // Every ClassLoader not registered as parallel-capable loads its classes while being locked on itself
    // (see sources of ClassLoader). ScexClassLoader loads classes from its virtual directory, which is not thread safe.
    // Compiler writes classes to this virtual directory, so synchronization over classLoader is needed during compilation.
    // So, compilation is effectively under two locks: ScexCompiler and ScexClassLoader, in that order.
    // There should not be deadlocks, because nobody locks first over ScexClassLoader and then over ScexCompiler.
    classLoader.synchronized {
      val global = this.global
      val run = new global.Run
      run.compileSources(List(sourceFile))
    }

    val duration = System.nanoTime - startTime
    logger.debug(s"Compilation took ${duration / 1000000}ms")

    val result = reporter.compileErrors()

    if (compilationCount > config.resetAfterCompilationCount) {
      reset()
    }

    result
  }

  protected def preprocess(exprDef: ExpressionDef): ExpressionDef =
    exprDef

  protected def compileExpression(exprDef: ExpressionDef): Try[RawExpression] = underLock {
    val preprocessedExprDef = preprocess(exprDef)
    val pkgName = newExpressionPackage()
    val (codeToCompile, offset) = expressionCode(preprocessedExprDef, pkgName)
    // every single expression has its own classloader and virtual directory
    val classLoader = createDedicatedClassLoader("(scex)")
    val sourceFile = new ExpressionSourceFile(preprocessedExprDef, pkgName, codeToCompile, offset)
    val sourceInfo = new SourceInfo(pkgName, codeToCompile, offset, offset + exprDef.expression.length,
      sourceFile.offsetToLine(offset) + 1, sourceFile.offsetToLine(offset + exprDef.expression.length - 1) + 2)
    val debugInfo = new ExpressionDebugInfo(exprDef, sourceInfo)

    def result =
      compile(sourceFile, classLoader, usedInExpressions = false) match {
        case Nil =>
          Class.forName(s"$pkgName.$ExpressionClassName", true, classLoader)
            .getConstructor(classOf[ExpressionDebugInfo]).newInstance(debugInfo)
            .asInstanceOf[RawExpression]

        case errors =>
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
  def compileSyntaxValidator(code: String): SyntaxValidator = underLock {
    val pkgName = newSyntaxValidatorPackage()
    val codeToCompile = wrapInSource(generateSyntaxValidator(code), pkgName)
    val sourceFile = new BatchSourceFile(pkgName, codeToCompile)

    compile(sourceFile, persistentClassLoader, usedInExpressions = true) match {
      case Nil =>
        instantiatePersistent[SyntaxValidator](s"$pkgName.$SyntaxValidatorClassName")
      case errors =>
        throw new CompilationFailedException(codeToCompile, errors)
    }
  }

  @throws[CompilationFailedException]
  def compileSymbolValidator(code: String): SymbolValidator = underLock {
    val pkgName = newSymbolValidatorPackage()
    val codeToCompile = wrapInSource(generateSymbolValidator(code), pkgName)
    val sourceFile = new BatchSourceFile(pkgName, codeToCompile)

    compile(sourceFile, persistentClassLoader, usedInExpressions = true) match {
      case Nil =>
        instantiatePersistent[SymbolValidator](s"$pkgName.$SymbolValidatorClassName")
      case errors =>
        throw new CompilationFailedException(codeToCompile, errors)
    }
  }

  /**
   * Compiles arbitrary Scala source file into a dedicated class loader and loads class with given fully qualified name
   * from that class loader.
   */
  def compileClass(code: String, name: String): Class[_] = underLock {
    val sourceFile = new BatchSourceFile("(unknown)", code)
    val classLoader = new ScexClassLoader(new VirtualDirectory("(unknown)", None), getClass.getClassLoader)

    compile(sourceFile, classLoader, usedInExpressions = false) match {
      case Nil =>
        Class.forName(name, true, classLoader)
      case errors =>
        throw new CompilationFailedException(code, errors)
    }
  }

  def reset(): Unit = underLock {
    init()
  }
}

object ScexCompiler {

  case class CompilationFailedException(source: String, errors: Seq[CompileError])
    extends RuntimeException(s"Compilation failed with ${pluralize(errors.size, "error")}:\n${errors.mkString("\n")}")

  case class CompileError(line: String, column: Int, msg: String) {
    override def toString = s"$msg:\n${line.stripLineEnd}\n${" " * (column - 1)}^"
  }

}
