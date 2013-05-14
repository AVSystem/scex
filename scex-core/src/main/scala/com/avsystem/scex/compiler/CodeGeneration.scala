package com.avsystem.scex.compiler

import java.lang.reflect._
import java.lang.{reflect => jlr}
import java.{util => ju, lang => jl}
import scala.Some
import scala.language.existentials
import scala.reflect.runtime.{universe => ru}

trait CodeGeneration extends JavaTypeParsing {

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

  protected val expressionClassName = "Expression"
  protected val profileObjectName = "Profile"

  protected def adapterName(clazz: Class[_]) =
    "Adapter_" + clazz.getName.replaceAll("\\.", "_")

  /**
   * Generates code of implicit view for given Java class that adds Scala-style getters
   * forwarding to existing Java-style getters of given class.
   */
  protected def generateJavaGetterAdapter(clazz: Class[_], full: Boolean): Option[String] = {
    // generate scala getters
    val javaGetters =
      if (full) clazz.getMethods else clazz.getDeclaredMethods

    val scalaGetters = javaGetters.collect {
      case method@JavaGetter(propName, booleanIsGetter) if Modifier.isPublic(method.getModifiers) =>
        val annot = if (booleanIsGetter) "@com.avsystem.scex.compiler.annotation.BooleanIsGetter\n" else ""
        s"${annot}def `$propName` = wrapped.${method.getName}\n"
    }

    if (full || scalaGetters.nonEmpty) {

      val classBody = scalaGetters.mkString

      val ClassExistential(polyTpe, typeVariables) = classToExistential(clazz)
      val wrappedTpe = javaTypeAsScalaType(polyTpe)
      val adapterWithGenerics = adapterName(clazz) + boundedTypeVariables(typeVariables)

      val result =
        s"""
        |@com.avsystem.scex.compiler.annotation.JavaGetterAdapter
        |class $adapterWithGenerics
        |  (val wrapped: $wrappedTpe) extends AnyVal {
        |
        |$classBody
        |}
        |
        """.stripMargin

      Some(result)

    } else None
  }

  protected def generateExpressionClass(
    profile: ExpressionProfile,
    expression: String,
    fullAdapterClassNameOpt: Option[String],
    profileObjectPkg: String,
    contextType: String,
    resultType: String) = {

    val header = Option(profile.expressionHeader).getOrElse("")

    val contextGetterAdapterCode = fullAdapterClassNameOpt match {
      case Some(fullAdapterClassName) =>
        s"""
        |@com.avsystem.scex.compiler.annotation.ContextAdapter
        |val _ctxa = new $fullAdapterClassName(_ctx)
        |import _ctxa._
        |
        """.stripMargin
      case None =>
        ""
    }

    s"""
    |class $expressionClassName extends (($contextType) => $resultType) {
    |  def apply(_ctx: $contextType): $resultType = {
    |    $header
    |    import $profileObjectPkg.$profileObjectName._
    |    import _ctx._
    |    $contextGetterAdapterCode
    |    com.avsystem.scex.validation.ExpressionValidator.validate[$contextType, $resultType](
    |$expression
    |    )
    |  }
    |}
    |
    """.stripMargin
  }

  protected def generateProfileObject(profile: ExpressionProfile) = {
    val adapters = profile.symbolValidator.referencedJavaClasses.flatMap { clazz =>
      generateJavaGetterAdapter(clazz, full = false).map { adapterCode =>
        val ClassExistential(polyTpe, typeVariables) = classToExistential(clazz)
        val wrappedTpe = javaTypeAsScalaType(polyTpe)
        val adapter = adapterName(clazz)
        val adapterWithGenerics = adapter + boundedTypeVariables(typeVariables)

        s"""
          |@com.avsystem.scex.compiler.annotation.JavaGetterAdapterConversion
          |implicit def $adapterWithGenerics(wrapped: $wrappedTpe) = new $adapter(wrapped)
          |$adapterCode
        """.stripMargin
      }
    }

    s"""
    |object $profileObjectName {
    |${adapters.mkString}
    |}
    |
    """.stripMargin
  }

  protected def wrapInSource(code: String, pkgName: String) = {
    s"""
      |package $pkgName
      |
      |$code
    """.stripMargin
  }

}