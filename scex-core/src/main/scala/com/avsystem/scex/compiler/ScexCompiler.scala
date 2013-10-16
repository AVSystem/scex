package com.avsystem.scex.compiler

import com.avsystem.scex.compiler.CodeGeneration._
import com.avsystem.scex.compiler.ScexCompiler._
import com.avsystem.scex.util.CommonUtils._
import com.avsystem.scex.validation.{SymbolValidator, SyntaxValidator, ExpressionMacroProcessor}
import com.avsystem.scex.{ExpressionContext, Expression}
import java.{util => ju, lang => jl}
import scala.collection.mutable.ListBuffer
import scala.ref.WeakReference
import scala.reflect.internal.util.{Position, SourceFile, BatchSourceFile}
import scala.reflect.io.VirtualDirectory
import scala.reflect.runtime.universe.TypeTag
import scala.tools.nsc.interpreter.AbstractFileClassLoader
import scala.tools.nsc.reporters.AbstractReporter
import scala.tools.nsc.{Global, Settings}
import scala.util.Try

trait ScexCompiler extends PackageGenerator {

  val config: ScexCompilerConfig

  protected case class ExpressionDef(
    profile: ExpressionProfile,
    expression: String,
    rootObjectClass: Class[_],
    contextType: String,
    resultType: String) {
  }

  class Reporter(val settings: Settings) extends AbstractReporter {
    private val errorsBuilder = new ListBuffer[CompileError]

    def compileErrors() = {
      errorsBuilder.result()
    }

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

  /**
   * Wrapper that avoids holding strong reference to actual compiled expression.
   */
  private class ExpressionWrapper[C <: ExpressionContext[_, _], T](exprDef: ExpressionDef) extends Expression[C, T] {
    var expressionRef = new WeakReference(loadRawExpression)

    private def loadRawExpression =
      compileExpression(exprDef).get.asInstanceOf[C => T]

    private def rawExpression: C => T =
      expressionRef.get match {
        case Some(expr) => expr
        case None =>
          expressionRef = new WeakReference(loadRawExpression)
          rawExpression
      }

    def apply(context: C) =
      rawExpression.apply(context)
  }

  protected class ScexClassLoader(val classfileDirectory: VirtualDirectory, parent: ClassLoader)
    extends AbstractFileClassLoader(classfileDirectory, parent)

  protected type RawExpression = Function[Any, Any]

  val settings = new Settings
  settings.usejavacp.value = true
  settings.exposeEmptyPackage.value = true

  private val reporter = new Reporter(settings)

  private var global: Global = _

  /**
   * Classloader for stuff that will be never reclaimed after compilation -
   * profiles, validators, custom util classes, etc.
   */
  private var persistentClassLoader: ScexClassLoader = _

  private var compilationCount: Int = _

  private def init() {
    compilationCount = 0
    global = new Global(settings, reporter)
    persistentClassLoader = new ScexClassLoader(new VirtualDirectory("(scex_persistent)", None), getClass.getClassLoader)
  }

  init()

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

  protected def compileFullJavaGetterAdapter(clazz: Class[_]): Try[String] = {
    val pkgName = newAdapterPackage()
    val codeToCompile = wrapInSource(generateJavaGetterAdapter(clazz, full = true).get, pkgName)
    val sourceFile = new BatchSourceFile("(scex adapter)", codeToCompile)

    def result = {
      compile(sourceFile, persistentClassLoader, shared = true) match {
        case Nil => pkgName
        case errors => throw new CompilationFailedException(codeToCompile, errors)
      }
    }

    Try(result)
  }

  protected def compileProfileObject(profile: ExpressionProfile): Try[String] = {
    val pkgName = newProfilePackage()
    val codeToCompile = wrapInSource(generateProfileObject(profile), pkgName)
    val sourceFile = new BatchSourceFile("(scex profile)", codeToCompile)

    def result = {
      compile(sourceFile, persistentClassLoader, shared = true) match {
        case Nil => pkgName
        case errors => throw new CompilationFailedException(codeToCompile, errors)
      }
    }

    Try(result)
  }

  protected def expressionCode(exprDef: ExpressionDef, pkgName: String): (String, Int) = {
    val ExpressionDef(profile, expression, rootObjectClass, contextType, resultType) = exprDef

    val fullAdapterClassNameOpt =
      if (profile.symbolValidator.referencedJavaClasses.contains(rootObjectClass)) {
        val adapterPkg = compileFullJavaGetterAdapter(exprDef.rootObjectClass).get
        val adapterClassName = adapterName(rootObjectClass)
        Some(s"$adapterPkg.$adapterClassName")
      } else None

    val profileObjectPkg = compileProfileObject(exprDef.profile).get
    val (expressionCode, offset) =
      generateExpressionClass(profile, expression, fullAdapterClassNameOpt, profileObjectPkg, contextType, resultType)

    wrapInSource(expressionCode, offset, pkgName)
  }

  protected def compileExpression(exprDef: ExpressionDef): Try[RawExpression] = synchronized {
    val pkgName = newExpressionPackage()
    val (codeToCompile, _) = expressionCode(exprDef, pkgName)
    // every single expression has its own classloader and virtual directory
    val classLoader = new ScexClassLoader(new VirtualDirectory("(scex)", None), persistentClassLoader)
    val sourceFile = new BatchSourceFile("(scex expression)", codeToCompile)

    def result =
      ExpressionMacroProcessor.profileVar.withValue(exprDef.profile) {
        compile(sourceFile, classLoader, shared = false) match {
          case Nil =>
            Class.forName(s"$pkgName.$ExpressionClassName", true, classLoader).newInstance.asInstanceOf[RawExpression]

          case errors =>
            throw new CompilationFailedException(codeToCompile, errors)
        }
      }

    Try(result)
  }

  protected def compile(sourceFile: SourceFile, classLoader: ScexClassLoader, shared: Boolean): Seq[CompileError] = {
    compilationCount += 1
    if (compilationCount > config.resetAfterCompilationCount) {
      reset()
    }

    settings.outputDirs.setSingleOutput(classLoader.classfileDirectory)
    reporter.reset()

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

    reporter.compileErrors()
  }

  /**
   * <p>Returns compiled string expression, ready to be evaluated. String expression is compiled as Scala string
   * interpolation. Example: <tt>Your name is $name and you are ${max(0, age)} years old</tt>.</p>
   *
   * <p>This method uses runtime scala reflection, you
   * may want to avoid using it until scala reflection becomes stable.
   * See <a href="https://issues.scala-lang.org/browse/SI/component/10400">open scala reflection issues</a>,
   * especially <a href="https://issues.scala-lang.org/browse/SI-6240">SI-6240</a>,
   * <a href="https://issues.scala-lang.org/browse/SI-6826">SI-6826</a> and
   * <a href="https://issues.scala-lang.org/browse/SI-6412">SI-6412</a>.</p>
   */
  @throws[CompilationFailedException]
  def getCompiledStringExpression[C <: ExpressionContext[_, _] : TypeTag](
    profile: ExpressionProfile,
    expression: String): Expression[C, String] = {

    import scala.reflect.runtime.universe._

    val mirror = typeTag[C].mirror
    val contextType = typeOf[C]
    val TypeRef(_, _, List(rootObjectType, _)) = contextType.baseType(typeOf[ExpressionContext[_, _]].typeSymbol)
    val rootObjectClass = mirror.runtimeClass(rootObjectType)

    getCompiledStringExpression(profile, expression, contextType.toString, rootObjectClass)
  }

  /**
   * <p>Returns compiled string expression, ready to be evaluated. String expression is compiled as Scala string
   * interpolation. Example: <tt>Your name is $name and you are ${max(0, age)} years old</tt>.</p>
   */
  @throws[CompilationFailedException]
  protected def getCompiledStringExpression[C <: ExpressionContext[_, _]](
    profile: ExpressionProfile,
    expression: String,
    contextType: String,
    contextClass: Class[_]): Expression[C, String] = {

    val stringExpr = "raw\"\"\"" + expression + "\"\"\""
    getCompiledExpression(profile, stringExpr, contextType, contextClass, "String")
  }

  /**
   * <p>Returns compiled expression ready to be evaluated.</p>
   *
   * <p>This method uses runtime scala reflection, you
   * may want to avoid using it until scala reflection becomes stable.
   *
   * See <a href="https://issues.scala-lang.org/browse/SI/component/10400">open scala reflection issues</a>,
   * especially <a href="https://issues.scala-lang.org/browse/SI-6240">SI-6240</a>,
   * <a href="https://issues.scala-lang.org/browse/SI-6826">SI-6826</a> and
   * <a href="https://issues.scala-lang.org/browse/SI-6412">SI-6412</a>.</p>
   */
  @throws[CompilationFailedException]
  def getCompiledExpression[C <: ExpressionContext[_, _] : TypeTag, T: TypeTag](
    profile: ExpressionProfile,
    expression: String): Expression[C, T] = {

    import scala.reflect.runtime.universe._

    val mirror = typeTag[C].mirror
    val contextType = typeOf[C]
    val resultType = typeOf[T]
    val TypeRef(_, _, List(rootObjectType, _)) = contextType.baseType(typeOf[ExpressionContext[_, _]].typeSymbol)
    val rootObjectClass = mirror.runtimeClass(rootObjectType)

    getCompiledExpression(profile, expression, contextType.toString, rootObjectClass, resultType.toString)
  }

  @throws[CompilationFailedException]
  protected def getCompiledExpression[C <: ExpressionContext[_, _], T](
    profile: ExpressionProfile,
    expression: String,
    contextType: String,
    rootObjectClass: Class[_],
    resultType: String): Expression[C, T] = {

    require(profile != null, "Profile cannot be null")
    require(expression != null, "Expression cannot be null")
    require(contextType != null, "Context type cannot be null")
    require(rootObjectClass != null, "Root object class cannot be null")
    require(resultType != null, "Result type cannot be null")

    new ExpressionWrapper(ExpressionDef(profile, expression, rootObjectClass, contextType, resultType))
  }

  @throws[CompilationFailedException]
  def compileSyntaxValidator(code: String): SyntaxValidator = synchronized {
    val pkgName = newSyntaxValidatorPackage()
    val codeToCompile = wrapInSource(generateSyntaxValidator(code), pkgName)
    val sourceFile = new BatchSourceFile("(scex syntax validator)", codeToCompile)

    compile(sourceFile, persistentClassLoader, shared = true) match {
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
    val sourceFile = new BatchSourceFile("(scex symbol validator)", codeToCompile)

    compile(sourceFile, persistentClassLoader, shared = true) match {
      case Nil =>
        instantiatePersistent[SymbolValidator](s"$pkgName.$SymbolValidatorClassName")
      case errors =>
        throw new CompilationFailedException(codeToCompile, errors)
    }
  }

  def reset() {
    synchronized {
      init()
    }
  }
}

object ScexCompiler {

  case class CompilationFailedException(source: String, errors: Seq[CompileError])
    extends RuntimeException(s"Compilation failed with ${pluralize(errors.size, "error")}:\n${errors.mkString("\n")}")

  case class CompileError(line: String, column: Int, msg: String) {
    override def toString = s"$msg:\n${line.stripLineEnd}\n${" " * (column - 1)}^"
  }

}
