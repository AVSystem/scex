package com.avsystem.scex

import java.{util => ju, lang => jl}
import scala.reflect.runtime.{universe => ru}
import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter.IMain
import scala.reflect.ClassTag
import scala.tools.nsc.interpreter.Results.Success
import scala.beans.BeanProperty
import scala.collection.JavaConverters._
import com.avsystem.scex.validation.ExpressionValidator
import java.util.concurrent.ConcurrentHashMap
import scala.language.existentials
import com.google.common.cache.{CacheLoader, CacheBuilder}
import scala.collection.mutable
import com.avsystem.scex.Utils._
import org.apache.commons.lang.StringEscapeUtils
import java.io.PrintWriter
import org.apache.commons.io.output.StringBuilderWriter

private object ExpressionCompiler {
  private val exprVal = "expr"

  private val javaGetterWrappersCache = CacheBuilder.newBuilder.build(new CacheLoader[Class[_], String] {
    def load(key: Class[_]): String =
      generateJavaGettersAdapter(key)
  })

  private case class ExpressionDef(
    profile: ExpressionProfile,
    expression: String,
    contextClass: Class[_],
    resultClass: Class[_]) {
  }

  private type RawExpression = Function[_, _]

  // extractor object for java getters
  private object JavaGetter {
    val getterPattern = "get([A-Z][a-z0-9_]*)+".r
    val booleanGetterPattern = "is([A-Z][a-z0-9_]*)+".r

    def unapply(symbol: ru.Symbol): Option[(String, Boolean)] =
      if (isJavaParameterlessMethod(symbol)) {

        def uncapitalize(str: String) =
          str.head.toLower + str.tail
        def isBoolOrBoolean(tpe: ru.Type) =
          tpe =:= ru.typeOf[Boolean] || tpe =:= ru.typeOf[jl.Boolean]

        symbol.name.toString match {
          case getterPattern(capitalizedName) =>
            Some((uncapitalize(capitalizedName), false))
          case booleanGetterPattern(capitalizedName) if isBoolOrBoolean(symbol.asMethod.returnType) =>
            Some((uncapitalize(capitalizedName), true))
          case _ => None
        }

      } else
        None
  }

  /**
   * Generates code of implicit view for given Java class that adds Scala-style getters
   * forwarding to existing Java-style getters of given class.
   */
  private def generateJavaGettersAdapter(clazz: Class[_]): String = {
    val classSymbol = ru.runtimeMirror(getClass.getClassLoader).classSymbol(clazz)

    if (!classSymbol.isJava) {
      return ""
    }

    val tpe = classSymbol.toType

    // generate scala getters
    val sb = new StringBuilder
    val alreadyWrapped = new mutable.HashSet[String]
    tpe.members.sorted.foreach { symbol =>
      symbol match {
        case JavaGetter(propName, booleanIsGetter) if !alreadyWrapped.contains(propName) =>
          alreadyWrapped += propName
          val annot = if (booleanIsGetter) "@com.avsystem.scex.BooleanIsGetter" else ""
          sb ++= s"$annot def `$propName` = wrapped.${symbol.name}\n"
        case _ => ()
      }
    }
    val propDefs = sb.mkString

    if (propDefs.isEmpty) {
      return ""
    }

    // generate class code
    val rawName = tpe.typeConstructor.toString
    val adapterName = "Adapter_" + rawName.replaceAll("\\.", "_")

    val typeParams = classSymbol.typeParams
    val genericTypes = if (typeParams.nonEmpty) typeParams.map(_.name).mkString("[", ",", "]") else ""

    val genericTypesWithBounds = if (typeParams.nonEmpty) {
      typeParams.map { param =>
        param.name.toString + param.typeSignature
      } mkString("[", ",", "]")
    } else ""

    s"""
      |implicit class $adapterName $genericTypesWithBounds
      |  (val wrapped: $rawName $genericTypes)
      |  extends AnyVal with com.avsystem.scex.JavaGettersAdapter {
      |$propDefs
      |}
      |
    """.stripMargin

  }
}

import ExpressionCompiler._

/**
 * Central class for expression compilation. Encapsulates Scala compiler, so it is
 * VERY heavy. Also, it is synchronized.
 */
class ExpressionCompiler {
  @BeanProperty var resetAfter: Int = 500

  private val settings = new Settings
  settings.usejavacp.value = true

  private val interpreterOutput = new jl.StringBuilder
  private val interpreter = new IMain(settings, new PrintWriter(new StringBuilderWriter(interpreterOutput)))

  private val expressionCache = new ConcurrentHashMap[ExpressionDef, RawExpression].asScala
  private val profileObjectsCache = CacheBuilder.newBuilder.build(new CacheLoader[ExpressionProfile, String] {
    def load(key: ExpressionProfile): String = {
      val ident = newProfileIdent()
      interpret(generateProfileObject(ident, key))
      ident
    }
  })

  private var profileIdentIndex = 0

  private def newProfileIdent() = {
    profileIdentIndex += 1
    s"__profile_$profileIdentIndex"
  }

  private def interpret(code: String) {
    try {
      interpreter.interpret(code) match {
        case Success => ()
        case _ => throw new RuntimeException("Compilation failure:\n" + interpreterOutput)
      }
    } finally {
      interpreterOutput.delete(0, interpreterOutput.length)
    }
  }

  private def clazzOf[T: ClassTag] = implicitly[ClassTag[T]].runtimeClass.asInstanceOf[Class[T]]

  def getCompiledStringExpression[C <: AnyRef : ClassTag](
    profile: ExpressionProfile,
    expression: String): Expression[C, String] = {

    getCompiledStringExpression(profile, expression, clazzOf[C])
  }

  def getCompiledStringExpression[C <: AnyRef](
    profile: ExpressionProfile,
    expression: String,
    contextClass: Class[C]): Expression[C, String] = {

    getCompiledExpression(profile, "s\"\"\"" + StringEscapeUtils.escapeJava(expression) + "\"\"\"", contextClass, classOf[String])
  }

  def getCompiledExpression[C <: AnyRef : ClassTag, R <: AnyRef : ClassTag](
    profile: ExpressionProfile,
    expression: String): Expression[C, R] = {

    getCompiledExpression[C, R](profile, expression, clazzOf[C], clazzOf[R])
  }

  /**
   * Returns compiled expression ready to be evaluated. Returned expression is actually a proxy that
   * looks up ExpressionCompiler's cache on every access (evaluation).
   *
   * @param profile expression profile to compile this expression with
   * @param expression the expression
   * @param contextClass expression context class
   * @param resultClass expression result class
   */
  def getCompiledExpression[C <: AnyRef, R <: AnyRef](
    profile: ExpressionProfile,
    expression: String,
    contextClass: Class[C],
    resultClass: Class[R]): Expression[C, R] = {

    require(profile != null, "Profile cannot be null")
    require(expression != null, "Expression cannot be null")
    require(contextClass != null, "Context class cannot be null")
    require(resultClass != null, "Result class cannot be null")

    val exprDef = ExpressionDef(profile, expression, contextClass, resultClass)

    // force eager compilation and return a proxy that will fetch compiled expression from ExpressionCompiler at each access
    if (getUsingCache(exprDef) != null)
      new Expression[C, R] {
        def apply(context: C) =
          getUsingCache(exprDef).asInstanceOf[C => R](context)
      }
    else
      null
  }

  /**
   * Manages cache. Returns cached expressions, puts into cache, clears cache and resets interpreter when needed.
   */
  private def getUsingCache(exprDef: ExpressionDef): RawExpression = {
    def compileAndPossiblyReset() = {
      val reset = expressionCache.size > resetAfter
      if (reset) {
        interpreter.reset()
        profileObjectsCache.invalidateAll()
      }
      val result = compileExpression(exprDef)
      if (reset) {
        expressionCache.clear()
      }
      if (result != null) {
        expressionCache(exprDef) = result
      }
      result
    }

    expressionCache.get(exprDef) match {
      case Some(result) => result
      case None => synchronized {
        expressionCache.get(exprDef) match {
          case Some(result) => result
          case None => compileAndPossiblyReset()
        }
      }
    }
  }

  /**
   * Actually uses interpreter to compile given expression into bytecode.
   */
  private def compileExpression(exprDef: ExpressionDef): RawExpression = {
    val mirror = ru.runtimeMirror(getClass.getClassLoader)

    def erasedType[A](clazz: Class[A]) =
      mirror.classSymbol(clazz).toType

    val profileObject = profileObjectsCache.get(exprDef.profile)
    val header = Option(exprDef.profile.expressionHeader).getOrElse("")

    val codeToCompile =
      s"""
        |val $exprVal: (${erasedType(exprDef.contextClass)} => ${erasedType(exprDef.resultClass)}) = {
        |  $header
        |  import $profileObject._
        |  __ctx => {
        |    import __ctx._
        |    com.avsystem.scex.validation.ExpressionValidator.validate(
        |      ${exprDef.expression}
        |    )
        |  }
        |}
      """.stripMargin

    ExpressionValidator.profile.withValue(exprDef.profile) {
      interpret(codeToCompile)
    }

    interpreter.valueOfTerm(exprVal).get.asInstanceOf[RawExpression]
  }

  private def generateProfileObject(ident: String, profile: ExpressionProfile): String = {
    val sb = new StringBuilder
    profile.wrappedJavaClasses foreach { clazz =>
      sb ++= javaGetterWrappersCache.get(clazz)
    }
    val wrappers = sb.mkString

    s"object $ident { $wrappers }"
  }
}
