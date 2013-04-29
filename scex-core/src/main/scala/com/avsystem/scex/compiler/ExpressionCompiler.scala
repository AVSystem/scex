package com.avsystem.scex.compiler

import ExpressionCompiler._
import TypeConverter._
import com.avsystem.scex.compiler.ExpressionCompiler.CompilationFailedException
import com.avsystem.scex.compiler.ExpressionCompiler.CompileError
import com.avsystem.scex.compiler.TypeConverter.ClassExistential
import com.avsystem.scex.util.CacheImplicits._
import com.avsystem.scex.validation.ExpressionValidator
import com.google.common.cache.{RemovalNotification, CacheBuilder}
import java.lang.reflect.{Modifier, Type, Method}
import java.{util => ju, lang => jl}
import org.apache.commons.lang.StringEscapeUtils
import scala.Some
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.existentials
import scala.ref.WeakReference
import scala.reflect.internal.util.{BatchSourceFile, Position}
import scala.reflect.io.VirtualDirectory
import scala.tools.nsc.interpreter.AbstractFileClassLoader
import scala.tools.nsc.reporters.AbstractReporter
import scala.tools.nsc.{Global, Settings}
import scala.util.{Failure, Success, Try}
import com.avsystem.scex.{TypeTag, Expression}

/**
 * Central class for expression compilation. Encapsulates Scala compiler, so it is
 * VERY heavy. Thread safe with compilation synchronized.
 */
class ExpressionCompiler(config: ExpressionCompilerConfig) {

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

  // extractor object for java getters
  private object JavaGetter {
    val getterPattern = "get([A-Z][a-z0-9_]*)+".r
    val booleanGetterPattern = "is([A-Z][a-z0-9_]*)+".r

    def unapply(method: Method): Option[(String, Boolean)] =
      if (method.getParameterTypes.isEmpty && method.getTypeParameters.isEmpty) {

        def uncapitalize(str: String) =
          str.head.toLower + str.tail
        def isBoolOrBoolean(clazz: Class[_]) =
          clazz == classOf[Boolean] || clazz == classOf[jl.Boolean]

        method.getName match {
          case getterPattern(capitalizedName) =>
            Some((uncapitalize(capitalizedName), false))
          case booleanGetterPattern(capitalizedName) if isBoolOrBoolean(method.getReturnType) =>
            Some((uncapitalize(capitalizedName), true))
          case _ => None
        }

      } else
        None
  }

  private case class ExpressionDef(
    profile: ExpressionProfile,
    expression: String,
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

  private val javaGetterAdaptersCache =
    CacheBuilder.newBuilder.build(generateJavaGetterAdapter: Class[_] => String)

  // when expression object is removed from cache, remove its compilation result too
  private def onExprRemove(notification: RemovalNotification[ExpressionDef, Try[RawExpression]]) {
    expressionCompilationResultsCache.invalidate(notification.getKey)
  }

  /**
   * CACHE BUILDER AND REMOVAL METHODS
   */

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
      val className = s"$pkgName.$expressionClassName"
      Class.forName(className, true, classLoader).newInstance.asInstanceOf[RawExpression]
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

  /**
   * Generates code of implicit view for given Java class that adds Scala-style getters
   * forwarding to existing Java-style getters of given class.
   */
  private def generateJavaGetterAdapter(clazz: Class[_]): String = {
    // generate scala getters
    val alreadyWrapped = new mutable.HashSet[String]

    val scalaGetters = clazz.getDeclaredMethods.collect {
      case method@JavaGetter(propName, booleanIsGetter)
        if Modifier.isPublic(method.getModifiers) && !alreadyWrapped.contains(propName) =>

        alreadyWrapped += propName
        val annot = if (booleanIsGetter) "@com.avsystem.scex.compiler.BooleanIsGetter\n" else ""
        s"${annot}def `$propName` = wrapped.${method.getName}\n"
    }

    if (scalaGetters.nonEmpty) {
      val classBody = scalaGetters.mkString

      val adapterName = "Adapter_" + clazz.getName.replaceAll("\\.", "_")

      val ClassExistential(polyType, typeVars) = classToExistential(clazz)
      val genericAdapter = adapterName + boundedTypeVariables(typeVars)
      val polyTypeRepr = javaTypeAsScalaType(polyType)

      s"""
      |implicit class $genericAdapter
      |  (val wrapped: $polyTypeRepr)
      |  extends AnyVal with com.avsystem.scex.compiler.JavaGettersAdapter {
      |
      |$classBody
      |}
      |
      """.stripMargin

    } else ""
  }

  private def compileProfileObject(profile: ExpressionProfile): Try[String] = {
    val pkgName = newProfilePackage()

    val adapters = profile.symbolValidator.referencedJavaClasses.map(javaGetterAdaptersCache.get).mkString

    val codeToCompile =
      s"""
      |package $pkgName
      |
      |object $profileObjectName {
      |$adapters
      |}
      |
      """.stripMargin

    compile("(scex profile)", codeToCompile) match {
      case Nil => Success(pkgName)
      case errors => Failure(new CompilationFailedException(codeToCompile, errors))
    }
  }

  private def compileExpression(exprDef: ExpressionDef): Try[String] = synchronized {
    val contextType = exprDef.contextType
    val resultType = exprDef.resultType

    val header = Option(exprDef.profile.expressionHeader).getOrElse("")

    val pkgName = newExpressionPackage()
    val profileObjectPkg = profileCompilationResultsCache.get(exprDef.profile).get

    val codeToCompile =
      s"""
      |package $pkgName
      |
      |class $expressionClassName extends (($contextType) => $resultType) {
      |  def apply(__ctx: $contextType): $resultType = {
      |    $header
      |    import $profileObjectPkg.$profileObjectName._
      |    import __ctx._
      |    com.avsystem.scex.validation.ExpressionValidator.validate(
      |${exprDef.expression}
      |    )
      |  }
      |}
      |
      """.stripMargin

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

    getCompiledExpression(profile, "s\"\"\"" + StringEscapeUtils.escapeJava(expression) + "\"\"\"", contextType: Type, classOf[String])
  }

  @throws[CompilationFailedException]
  def getCompiledStringExpression[C](
    profile: ExpressionProfile,
    expression: String,
    contextType: TypeTag[C]): Expression[C, String] = {

    getCompiledExpression(profile, "s\"\"\"" + StringEscapeUtils.escapeJava(expression) + "\"\"\"", contextType: Type, classOf[String])
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

    val exprDef = ExpressionDef(profile, expression, javaTypeAsScalaType(contextType), javaTypeAsScalaType(resultType))

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

  private val expressionClassName = "Expression"
  private val profileObjectName = "Profile"

}