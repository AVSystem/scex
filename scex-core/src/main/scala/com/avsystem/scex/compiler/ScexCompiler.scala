package com.avsystem.scex.compiler

import CodeGeneration._
import JavaTypeParsing._
import com.avsystem.scex.compiler.ScexCompiler.CompilationFailedException
import com.avsystem.scex.compiler.ScexCompiler.CompileError
import com.avsystem.scex.util.CacheImplicits._
import com.avsystem.scex.util.CommonUtils._
import com.avsystem.scex.validation.{SymbolValidator, SyntaxValidator, ExpressionValidator}
import com.avsystem.scex.{TypeTag, Expression}
import com.google.common.cache.CacheBuilder
import java.lang.reflect.Type
import java.util.concurrent.TimeUnit
import java.{util => ju, lang => jl}
import org.apache.commons.lang.StringEscapeUtils
import scala.Some
import scala.language.existentials
import scala.ref.WeakReference
import scala.reflect.internal.util.{BatchSourceFile, Position}
import scala.reflect.io.VirtualDirectory
import scala.tools.nsc.interpreter.AbstractFileClassLoader
import scala.tools.nsc.reporters.AbstractReporter
import scala.tools.nsc.{Global, Settings}
import scala.util.{Failure, Success, Try}

/**
 * Central class for expression compilation. Encapsulates Scala compiler, so it is
 * VERY heavy - it is probably undesirable to have more that one instance of ScexCompiler in JVM.
 * It can be used safely from multiple threads. Expression compilation,
 * classloading and instantiation is synchronized and results are cached.
 */
class ScexCompiler(config: ScexCompilerConfig) {
  compiler =>

  /**
   * Wrapper that avoids holding strong reference to actual compiled expression.
   */
  private class ExpressionWrapper[C, R](exprDef: ExpressionDef) extends Expression[C, R] {
    var expressionRef = new WeakReference(loadRawExpression)

    private def loadRawExpression =
      expressionCache.get(exprDef).get.asInstanceOf[C => R]

    private def rawExpression: C => R =
      expressionRef.get match {
        case Some(expr) => expr
        case None =>
          expressionRef = new WeakReference(loadRawExpression)
          rawExpression
      }

    def apply(context: C) =
      rawExpression.apply(context)
  }

  private class ScexClassLoader(val classfileDirectory: VirtualDirectory, parent: ClassLoader)
    extends AbstractFileClassLoader(classfileDirectory, parent)

  private val settings = new Settings
  settings.usejavacp.value = true
  settings.exposeEmptyPackage.value = true

  /**
   * Scala compiler issues reporter
   */
  private object reporter extends AbstractReporter {
    private val errorsBuilder = IndexedSeq.newBuilder[CompileError]

    def compileErrors() = {
      val result = errorsBuilder.result()
      reset()
      result
    }

    val settings: Settings = ScexCompiler.this.settings

    def display(pos: Position, msg: String, severity: Severity) {
      if (severity == ERROR) {
        errorsBuilder += CompileError(pos.lineContent, if (pos.isDefined) pos.column else 1, msg)
      }
    }

    def displayPrompt() {}

    override def reset() {
      super.reset()
      errorsBuilder.clear()
    }
  }

  private case class ExpressionDef(
    profile: ExpressionProfile,
    expression: String,
    contextClass: Class[_],
    contextType: String,
    resultType: String) {
  }

  private type RawExpression = Function[Any, Any]

  private var global: Global = _

  /**
   * Classloader for stuff that will be never reclaimed after compilation -
   * profiles, validators, custom util classes, etc.
   */
  private var persistentClassLoader: ScexClassLoader = _

  private var compilationCount: Int = _

  private def instantiatePersistent[T](className: String) =
    Class.forName(className, true, persistentClassLoader).newInstance.asInstanceOf[T]

  /**
   * VARIOUS CACHES
   */

  private val expressionCache = CacheBuilder.newBuilder
    .expireAfterAccess(config.expressionExpirationTime, TimeUnit.MILLISECONDS)
    .build[ExpressionDef, Try[RawExpression]](compileExpression _)

  // holds names of packages to which profiles are compiled
  private val profileCompilationResultsCache =
    CacheBuilder.newBuilder.build[ExpressionProfile, Try[String]](compileProfileObject _)

  // holds code of implicit adapters over Java classes that add Scala-style getters to Java bean getters
  private val fullJavaGetterAdaptersCache =
    CacheBuilder.newBuilder.build[Class[_], Try[String]](compileFullJavaGetterAdapter _)

  def reset() {
    synchronized {
      compilationCount = 0
      global = new Global(settings, reporter)
      persistentClassLoader = new ScexClassLoader(new VirtualDirectory("(scex_persistent)", None), getClass.getClassLoader)
      expressionCache.invalidateAll()
      profileCompilationResultsCache.invalidateAll()
      fullJavaGetterAdaptersCache.invalidateAll()
    }
  }

  reset()

  /**
   * CACHE BUILDER AND REMOVAL METHODS
   */

  private var idx = 0

  private def newExpressionPackage() = {
    idx += 1
    "_scex_expr$" + idx
  }

  private def newProfilePackage() = {
    idx += 1
    "_scex_profile$" + idx
  }

  private def newAdapterPackage() = {
    idx += 1
    "_scex_adapter$" + idx
  }

  private def newSyntaxValidatorPackage() = {
    idx += 1
    "_scex_syntax_validator$" + idx
  }

  private def newSymbolValidatorPackage() = {
    idx += 1
    "_scex_symbol_validator$" + idx
  }

  private def compileFullJavaGetterAdapter(clazz: Class[_]): Try[String] = {
    val pkgName = newAdapterPackage()

    val codeToCompile =
      wrapInSource(generateJavaGetterAdapter(clazz, full = true).get, pkgName)

    def result = compile("(scex adapter)", codeToCompile, persistentClassLoader) match {
      case Nil => pkgName
      case errors => throw new CompilationFailedException(codeToCompile, errors)
    }

    Try(result)
  }

  private def compileProfileObject(profile: ExpressionProfile): Try[String] = {
    val pkgName = newProfilePackage()

    val codeToCompile =
      wrapInSource(generateProfileObject(profile), pkgName)

    def result = compile("(scex profile)", codeToCompile, persistentClassLoader) match {
      case Nil => pkgName
      case errors => throw new CompilationFailedException(codeToCompile, errors)
    }

    Try(result)
  }

  private def compileExpression(exprDef: ExpressionDef): Try[RawExpression] = synchronized {
    val ExpressionDef(profile, expression, contextClass, contextType, resultType) = exprDef

    val fullAdapterClassNameOpt =
      if (profile.symbolValidator.referencedJavaClasses.contains(contextClass)) {
        val adapterPkg = fullJavaGetterAdaptersCache.get(exprDef.contextClass).get
        val adapterClassName = adapterName(contextClass)
        Some(s"$adapterPkg.$adapterClassName")
      } else None

    val profileObjectPkg = profileCompilationResultsCache.get(exprDef.profile).get

    val pkgName = newExpressionPackage()

    val codeToCompile = wrapInSource(generateExpressionClass(
      profile, expression, fullAdapterClassNameOpt, profileObjectPkg, contextType, resultType), pkgName)

    // every single expression has its own classloader and virtual directory
    val classLoader = new ScexClassLoader(new VirtualDirectory("(scex)", None), persistentClassLoader)

    def result =
      ExpressionValidator.profileVar.withValue(exprDef.profile) {
        compile("(scex expression)", codeToCompile, classLoader) match {
          case Nil =>
            Class.forName(s"$pkgName.$ExpressionClassName", true, classLoader).newInstance.asInstanceOf[RawExpression]

          case errors =>
            throw new CompilationFailedException(codeToCompile, errors)
        }
      }

    Try(result)
  }

  private def compile(name: String, code: String, classLoader: ScexClassLoader): Seq[CompileError] = {
    settings.outputDirs.setSingleOutput(classLoader.classfileDirectory)

    println("Compiling: " + code)

    // Every ClassLoader not registered as parallel-capable loads its classes while being locked on itself
    // (see sources of ClassLoader). ScexClassLoader loads classes from its virtual directory, which is not thread safe.
    // Compiler writes classes to this virtual directory, so synchronization over classLoader is needed during compilation.
    // So, compilation is effectively under two locks: ScexCompiler and ScexClassLoader, in that order.
    // There should not be deadlocks, because nobody locks first over ScexClassLoader and then over ScexCompiler.
    classLoader.synchronized {
      val global = this.global
      val run = new global.Run
      run.compileSources(List(new BatchSourceFile(name, code)))
    }

    compilationCount += 1
    if (compilationCount >= config.resetAfterCompilationCount) {
      reset()
    }

    reporter.compileErrors()
  }

  // lots of adapter methods for Java usage

  @throws[CompilationFailedException]
  def getCompiledStringExpression[C](
    profile: ExpressionProfile,
    expression: String,
    contextType: Class[C]): Expression[C, String] = {

    val stringExpr = "s\"\"\"" + StringEscapeUtils.escapeJava(expression) + "\"\"\""
    getCompiledExpression(profile, stringExpr, contextType: Type, classOf[String])
  }

  @throws[CompilationFailedException]
  def getCompiledStringExpression[C](
    profile: ExpressionProfile,
    expression: String,
    contextType: TypeTag[C]): Expression[C, String] = {

    val stringExpr = "s\"\"\"" + StringEscapeUtils.escapeJava(expression) + "\"\"\""
    getCompiledExpression(profile, stringExpr, contextType: Type, classOf[String])
  }

  @throws[CompilationFailedException]
  def getCompiledExpression[C, R](
    profile: ExpressionProfile,
    expression: String,
    contextType: Class[C],
    resultType: Class[R]): Expression[C, R] = {

    getCompiledExpression[C, R](profile, expression, contextType: Type, resultType: Type)
  }

  @throws[CompilationFailedException]
  def getCompiledExpression[C, R](
    profile: ExpressionProfile,
    expression: String,
    contextType: Class[C],
    resultType: TypeTag[R]): Expression[C, R] = {

    getCompiledExpression[C, R](profile, expression, contextType: Type, resultType: Type)
  }

  @throws[CompilationFailedException]
  def getCompiledExpression[C, R](
    profile: ExpressionProfile,
    expression: String,
    contextType: TypeTag[C],
    resultType: Class[R]): Expression[C, R] = {

    getCompiledExpression[C, R](profile, expression, contextType: Type, resultType: Type)
  }

  @throws[CompilationFailedException]
  def getCompiledExpression[C, R](
    profile: ExpressionProfile,
    expression: String,
    contextType: TypeTag[C],
    resultType: TypeTag[R]): Expression[C, R] = {

    getCompiledExpression[C, R](profile, expression, contextType: Type, resultType: Type)
  }

  /**
   * Returns compiled expression ready to be evaluated. Returned expression is actually a proxy that
   * looks up ScexCompiler's cache on every access (evaluation).
   *
   * @param profile expression profile to compile this expression with
   * @param expression the expression
   * @param contextType expression context class
   * @param resultType expression result class
   */
  @throws[CompilationFailedException]
  def getCompiledExpression[C, R](
    profile: ExpressionProfile,
    expression: String,
    contextType: Type,
    resultType: Type): Expression[C, R] = {

    require(profile != null, "Profile cannot be null")
    require(expression != null, "Expression cannot be null")
    require(contextType != null, "Context type cannot be null")
    require(resultType != null, "Result type cannot be null")

    val exprDef = ExpressionDef(profile, expression, erasureOf(contextType),
      javaTypeAsScalaType(contextType), javaTypeAsScalaType(resultType))

    new ExpressionWrapper(exprDef)
  }

  @throws[CompilationFailedException]
  def compileSyntaxValidator(code: String): SyntaxValidator = synchronized {
    val pkgName = newSyntaxValidatorPackage()
    val codeToCompile = wrapInSource(generateSyntaxValidator(code), pkgName)

    compile("(scex syntax validator)", codeToCompile, persistentClassLoader) match {
      case Nil =>
        instantiatePersistent[SyntaxValidator](s"$pkgName.$SyntaxValidatorClassName")
      case errors =>
        throw new CompilationFailedException(codeToCompile, errors)
    }
  }

  @throws[CompilationFailedException]
  def compileSymbolValidator(code: String): SymbolValidator = synchronized {
    val pkgName = newSymbolValidatorPackage()
    val codeToCompile = wrapInSource(generateSymbolValidator(code), pkgName)

    compile("(scex symbol validator)", codeToCompile, persistentClassLoader) match {
      case Nil =>
        instantiatePersistent[SymbolValidator](s"$pkgName.$SymbolValidatorClassName")
      case errors =>
        throw new CompilationFailedException(codeToCompile, errors)
    }
  }
}

object ScexCompiler {

  case class CompileError(line: String, column: Int, msg: String) {
    override def toString = s"$msg:\n${line.stripLineEnd}\n${" " * (column - 1)}^"
  }

  case class CompilationFailedException(source: String, errors: Seq[CompileError])
    extends Exception(s"Compilation failed with ${pluralize(errors.size, "error")}:\n${errors.mkString("\n")}")

}
