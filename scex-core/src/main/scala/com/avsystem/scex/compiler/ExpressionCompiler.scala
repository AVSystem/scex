package com.avsystem.scex.compiler

import com.avsystem.scex.compiler.ExpressionCompiler.CompilationFailedException
import com.avsystem.scex.compiler.ExpressionCompiler.CompileError
import com.avsystem.scex.util.CacheImplicits._
import com.avsystem.scex.validation.ExpressionValidator
import com.avsystem.scex.{TypeTag, Expression}
import com.google.common.cache.{RemovalNotification, CacheBuilder}
import java.lang.reflect.Type
import java.{util => ju, lang => jl}
import org.apache.commons.lang.StringEscapeUtils
import scala.Some
import scala.collection.mutable.ListBuffer
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
 * VERY heavy. Thread safe with compilation synchronized.
 */
class ExpressionCompiler(config: ExpressionCompilerConfig) extends CodeGeneration {

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

  case class ExpressionDef(
    profile: ExpressionProfile,
    expression: String,
    contextClass: Class[_],
    contextType: String,
    resultType: String)

  private type RawExpression = Function[Any, Nothing]

  private val global = new Global(settings, reporter)

  private var classLoader = new AbstractFileClassLoader(virtualDirectory, getClass.getClassLoader)

  //TODO: profiles, cache expiration, classLoader resetting, validator compilation

  /**
   * VARIOUS CACHES
   */

  private val expressionCache = CacheBuilder.newBuilder
    .removalListener(onExprRemove _).build[ExpressionDef, Try[RawExpression]](loadCompiledRawExpression _)

  // holds names of packages to which expressions are compiled
  private val expressionCompilationResultsCache =
    CacheBuilder.newBuilder.removalListener(onCompilationResultRemoval[ExpressionDef] _)
      .build[ExpressionDef, Try[String]](compileExpression _)

  // holds names of packages to which profiles are compiled
  private val profileCompilationResultsCache =
    CacheBuilder.newBuilder.removalListener(onCompilationResultRemoval[ExpressionProfile] _)
      .build[ExpressionProfile, Try[String]](compileProfileObject _)

  private val fullJavaGetterAdaptersCache =
    CacheBuilder.newBuilder.removalListener(onCompilationResultRemoval[Class[_]] _)
      .build[Class[_], Try[String]](compileFullJavaGetterAdapter _)

  /**
   * CACHE BUILDER AND REMOVAL METHODS
   */

  // when expression object is removed from cache, remove its compilation result too
  private def onExprRemove(notification: RemovalNotification[ExpressionDef, Try[RawExpression]]) {
    expressionCompilationResultsCache.invalidate(notification.getKey)
  }

  // when cache entry expires, remove corresponding classfiles from virtualDirectory
  private def onCompilationResultRemoval[K](notification: RemovalNotification[K, Try[String]]) {
    notification.getValue match {
      case Success(pkgName) =>
        virtualDirectory.lookupPath(pkgName.replaceAllLiterally(".", "/"), directory = true).delete()
      case _ => ()
    }
  }

  private def loadCompiledRawExpression(exprDef: ExpressionDef): Try[RawExpression] = synchronized {
    expressionCompilationResultsCache.get(exprDef).map { pkgName =>
      Class.forName(s"$pkgName.$expressionClassName", true, classLoader).newInstance.asInstanceOf[RawExpression]
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

  private def compileExpression(exprDef: ExpressionDef): Try[String] = synchronized {
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
        case Nil => Success(pkgName)
        case errors => Failure(new CompilationFailedException(codeToCompile, errors))
      }
    }
  }

  private def compile(name: String, code: String): List[CompileError] = {
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

    // return wrapper over a weak reference to actual expression
    // this allows the actual expression to be GCed when ExpressionCompiler is doing cleanup
    // without which the classloader will not be GCed and its classes unloaded
    new Expression[C, R] {
      var rawExpressionRef = new WeakReference(loadRawExpression)

      def loadRawExpression = expressionCache.get(exprDef).get

      def rawExpression = rawExpressionRef.get match {
        case Some(expr) => expr
        case None =>
          val result = loadRawExpression
          rawExpressionRef = new WeakReference(result)
          result
      }

      def apply(context: C): R =
        rawExpression.asInstanceOf[C => R].apply(context)
    }
  }
}

object ExpressionCompiler {

  case class CompileError(line: String, column: Int, msg: String) {
    override def toString = s"$msg:\n${line.stripLineEnd}\n${" " * (column - 1)}^"
  }

  case class CompilationFailedException(source: String, errors: List[CompileError])
    extends Exception(s"Compilation failed with ${errors.size} errors:\n${errors.mkString("\n")}")

}