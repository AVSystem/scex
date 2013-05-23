package com.avsystem.scex.compiler

import com.avsystem.scex.compiler.ExpressionCompiler.CompilationFailedException
import com.avsystem.scex.compiler.ExpressionCompiler.CompileError
import com.avsystem.scex.util.CacheImplicits._
import com.avsystem.scex.validation.{SymbolValidator, SyntaxValidator, ExpressionValidator}
import com.avsystem.scex.{TypeTag, Expression}
import com.google.common.cache.{RemovalNotification, CacheBuilder}
import java.lang.reflect.Type
import java.util.concurrent.TimeUnit
import java.{util => ju, lang => jl}
import org.apache.commons.lang.StringEscapeUtils
import scala.Some
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.existentials
import scala.reflect.internal.util.{BatchSourceFile, Position}
import scala.reflect.io.VirtualDirectory
import scala.tools.nsc.interpreter.AbstractFileClassLoader
import scala.tools.nsc.reporters.AbstractReporter
import scala.tools.nsc.{Global, Settings}
import scala.util.{Failure, Success, Try}
import com.avsystem.scex.util.CommonUtils._
import JavaTypeParsing._
import CodeGeneration._

/**
 * Central class for expression compilation. Encapsulates Scala compiler, so it is
 * VERY heavy - it is probably undesirable to have more that one instance of ExpressionCompiler in JVM.
 * It can be used safely from multiple threads. Expression compilation,
 * classloading and instantiation is synchronized and results are cached.
 */
class ExpressionCompiler(config: ExpressionCompilerConfig) {

  private class ExpressionHolder(val pkgName: String) {
    @volatile private var _rawExpression = loadCompiledRawExpression(pkgName)

    def rawExpression = {
      if (_rawExpression == null) {
        _rawExpression = loadCompiledRawExpression(pkgName)
      }
      _rawExpression
    }

    def reset() {
      _rawExpression = null
    }
  }

  private val virtualDirectory = new VirtualDirectory("(scex)", None)

  private val settings = new Settings
  settings.usejavacp.value = true
  settings.exposeEmptyPackage.value = true
  settings.outputDirs.setSingleOutput(virtualDirectory)

  private object reporter extends AbstractReporter {
    private val errorsBuffer = new ListBuffer[CompileError]

    def compileErrors = errorsBuffer.result()

    val settings: Settings = ExpressionCompiler.this.settings

    def display(pos: Position, msg: String, severity: Severity) {
      if (severity == ERROR) {
        errorsBuffer += CompileError(pos.lineContent, if (pos.isDefined) pos.column else 1, msg)
      }
    }

    def displayPrompt() {}

    override def reset() {
      super.reset()
      errorsBuffer.clear()
    }
  }

  private case class ExpressionDef(
    profile: ExpressionProfile,
    expression: String,
    contextClass: Class[_],
    contextType: String,
    resultType: String) {
  }

  private type RawExpression = Function[Any, Nothing]

  private def createGlobal = new Global(settings, reporter)

  private val global = createGlobal

  private def createClassLoader = new AbstractFileClassLoader(virtualDirectory, getClass.getClassLoader)

  private var classLoader = createClassLoader

  // class loader that will never be cleaned; used mainly for on-demand compiled syntax/symbol validators
  private val persistentClassLoader = createClassLoader

  /**
   * VARIOUS CACHES
   */

  private val holdersSet = new mutable.HashSet[ExpressionHolder]

  private val expressionCache = CacheBuilder.newBuilder
    .expireAfterAccess(config.expressionExpirationTime, TimeUnit.MILLISECONDS)
    .removalListener(onExprRemove _)
    .build[ExpressionDef, Try[ExpressionHolder]](compileExpression _)

  // holds names of packages to which profiles are compiled
  private val profileCompilationResultsCache =
    CacheBuilder.newBuilder.build[ExpressionProfile, Try[String]](compileProfileObject _)

  private val fullJavaGetterAdaptersCache =
    CacheBuilder.newBuilder.build[Class[_], Try[String]](compileFullJavaGetterAdapter _)

  /**
   * CACHE BUILDER AND REMOVAL METHODS
   */

  // clean up stuff associated with expression holder, mainly compiled classfile
  private def onExprRemove(notification: RemovalNotification[ExpressionDef, Try[ExpressionHolder]]) {
    notification.getValue match {
      case Success(holder) => synchronized {
        holdersSet.remove(holder)
        virtualDirectory.lookupPath(holder.pkgName.replaceAllLiterally(".", "/"), directory = true).delete()
      }
      case _ => ()
    }
  }

  private def loadCompiledRawExpression(pkgName: String): RawExpression = synchronized {
    Class.forName(s"$pkgName.$ExpressionClassName", true, classLoader).newInstance.asInstanceOf[RawExpression]
  }

  def cleanUp() {
    expressionCache.cleanUp()
    synchronized {
      classLoader = createClassLoader
      holdersSet.foreach(_.reset())
    }
  }

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

    compile("(scex adapter)", codeToCompile) match {
      case Nil => Success(pkgName)
      case errors => Failure(new CompilationFailedException(codeToCompile, errors))
    }
  }

  private def compileProfileObject(profile: ExpressionProfile): Try[String] = {
    val pkgName = newProfilePackage()

    val codeToCompile =
      wrapInSource(generateProfileObject(profile), pkgName)

    compile("(scex profile)", codeToCompile) match {
      case Nil => Success(pkgName)
      case errors => Failure(new CompilationFailedException(codeToCompile, errors))
    }
  }

  private def compileExpression(exprDef: ExpressionDef): Try[ExpressionHolder] = synchronized {
    val ExpressionDef(profile, expression, contextClass, contextType, resultType) = exprDef

    val fullAdapterClassNameOpt =
      if (profile.symbolValidator.referencedJavaClasses.contains(contextClass)) {
        val adapterPkg = fullJavaGetterAdaptersCache.get(exprDef.contextClass).get
        val adapterClassName = adapterName(contextClass)
        Some(s"$adapterPkg.$adapterClassName")
      } else {
        None
      }

    val profileObjectPkg = profileCompilationResultsCache.get(exprDef.profile).get

    val pkgName = newExpressionPackage()

    val codeToCompile = wrapInSource(generateExpressionClass(
      profile, expression, fullAdapterClassNameOpt, profileObjectPkg, contextType, resultType), pkgName)

    ExpressionValidator.profileVar.withValue(exprDef.profile) {
      compile("(scex expression)", codeToCompile) match {
        case Nil =>
          val holder = new ExpressionHolder(pkgName)
          holdersSet += holder
          Success(holder)

        case errors =>
          Failure(new CompilationFailedException(codeToCompile, errors))
      }
    }
  }

  private def compile(name: String, code: String): List[CompileError] = {
    val global = this.global
    val run = new global.Run
    run.compileSources(List(new BatchSourceFile(name, code)))
    reporter.compileErrors
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
   * looks up ExpressionCompiler's cache on every access (evaluation).
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

    def loadExpressionHolder =
      expressionCache.get(exprDef).get

    // force expression compilation
    loadExpressionHolder

    // return delegate that loads actual expression from cache on every access
    // this way actual expression can be GCed when expression compiler is cleaned up
    new Expression[C, R] {
      def apply(context: C): R =
        loadExpressionHolder.rawExpression.asInstanceOf[C => R].apply(context)
    }
  }

  @throws[CompilationFailedException]
  def compileSyntaxValidator(code: String): SyntaxValidator = synchronized {
    val pkgName = newSyntaxValidatorPackage()
    val codeToCompile = wrapInSource(generateSyntaxValidator(code), pkgName)

    compile("(scex syntax validator)", codeToCompile) match {
      case Nil =>
        Class.forName(s"$pkgName.$SyntaxValidatorClassName", true, persistentClassLoader)
          .newInstance.asInstanceOf[SyntaxValidator]
      case errors =>
        throw new CompilationFailedException(codeToCompile, errors)
    }
  }

  @throws[CompilationFailedException]
  def compileSymbolValidator(code: String): SymbolValidator = synchronized {
    val pkgName = newSymbolValidatorPackage()
    val codeToCompile = wrapInSource(generateSymbolValidator(code), pkgName)

    compile("(scex symbol validator)", codeToCompile) match {
      case Nil =>
        Class.forName(s"$pkgName.$SymbolValidatorClassName", true, persistentClassLoader)
          .newInstance.asInstanceOf[SymbolValidator]
      case errors =>
        throw new CompilationFailedException(codeToCompile, errors)
    }
  }
}

object ExpressionCompiler {

  case class CompileError(line: String, column: Int, msg: String) {
    override def toString = s"$msg:\n${line.stripLineEnd}\n${" " * (column - 1)}^"
  }

  case class CompilationFailedException(source: String, errors: List[CompileError])
    extends Exception(s"Compilation failed with ${pluralize(errors.size, "error")}:\n${errors.mkString("\n")}")

}