package com.avsystem.scex
package compiler

import JavaTypeParsing._
import java.lang.reflect.{Modifier, Method}
import java.{util => ju, lang => jl}
import scala.Some
import scala.language.existentials
import util.CommonUtils._

object CodeGeneration {

  // extractor object for java getters
  private object JavaGetter {
    def unapply(method: Method): Option[(String, Boolean)] =
      if (method.getParameterTypes.isEmpty && method.getTypeParameters.isEmpty) {

        def uncapitalize(str: String) =
          str.head.toLower + str.tail
        def isBoolOrBoolean(clazz: Class[_]) =
          clazz == classOf[Boolean] || clazz == classOf[jl.Boolean]

        method.getName match {
          case name@BeanGetterNamePattern(capitalizedName, _) if name != "getClass" =>
            Some((uncapitalize(capitalizedName), false))
          case BooleanBeanGetterNamePattern(capitalizedName, _) if isBoolOrBoolean(method.getReturnType) =>
            Some((uncapitalize(capitalizedName), true))
          case _ => None
        }

      } else None

  }

  val ExpressionClassName = "Expression"
  val ProfileObjectName = "Profile"
  val SyntaxValidatorClassName = "SyntaxValidator"
  val SymbolValidatorClassName = "SymbolValidator"
  val ContextSymbol = "_ctx"
  val VariablesSymbol = "_vars"
  val RootSymbol = "_root"
  val CompilerPkg = "com.avsystem.scex.compiler"
  val AnnotationPkg = s"$CompilerPkg.annotation"
  val MacroProcessor = s"$CompilerPkg.ExpressionMacroProcessor"
  val InterpolationOpen = "t\"\"\""
  val InterpolationClose = "\"\"\""

  def adapterName(clazz: Class[_]) =
    "Adapter_" + clazz.getName.replaceAll("\\.", "_")

  /**
   * Generates code of implicit view for given Java class that adds Scala-style getters
   * forwarding to existing Java-style getters of given class.
   */
  def generateJavaGetterAdapter(clazz: Class[_], full: Boolean): Option[String] = {
    // generate scala getters
    val javaGetters =
      if (full) clazz.getMethods else clazz.getDeclaredMethods

    val scalaGetters = javaGetters.collect {
      case method@JavaGetter(propName, booleanIsGetter) if Modifier.isPublic(method.getModifiers) =>
        val annot = if (booleanIsGetter) s"@$AnnotationPkg.BooleanIsGetter\n" else ""
        s"${annot}def `$propName` = _wrapped.${method.getName}\n"
    }

    if (full || scalaGetters.nonEmpty) {

      val classBody = scalaGetters.mkString

      val ExistentialType(polyTpe, typeVariables) = classToExistential(clazz)
      val generics = if (typeVariables.nonEmpty) typeVariableDeclarations(typeVariables).mkString("[", ", ", "]") else ""
      val wrappedTpe = javaTypeAsScalaType(polyTpe)
      val adapterWithGenerics = adapterName(clazz) + generics

      val result =
        s"""
        |@$AnnotationPkg.JavaGetterAdapter
        |class $adapterWithGenerics
        |  (@$AnnotationPkg.WrappedInAdapter val _wrapped: $wrappedTpe) extends AnyVal {
        |
        |$classBody
        |}
        |
        """.stripMargin

      Some(result)

    } else None
  }

  def generateExpressionClass(
    exprDef: ExpressionDef,
    fullAdapterClassNameOpt: Option[String],
    profileObjectPkg: String) = {

    val ExpressionDef(profile, template, setter, expression, header, _, contextType, resultType) = exprDef

    val resultOrSetterType = if(setter) s"com.avsystem.scex.Setter[$resultType]" else resultType

    val profileHeader = Option(profile.expressionHeader).getOrElse("")
    val additionalHeader = Option(header).getOrElse("")

    val rootGetterAdapterCode = fullAdapterClassNameOpt match {
      case Some(fullAdapterClassName) =>
        s"""
        |@$AnnotationPkg.RootAdapter
        |val _adapted_root = new $fullAdapterClassName($RootSymbol)
        |import _adapted_root._
        |""".stripMargin
      case None =>
        ""
    }

    val interpolationPrefix = if (template) InterpolationOpen else ""
    val interpolationPostfix = if (template) InterpolationClose else ""
    val setterConversion = if(setter) s"$MacroProcessor.asSetter" else ""

    //_result is needed because: https://groups.google.com/forum/#!topic/scala-user/BAK-mU7o6nM
    val prefix =
      s"""
        |
        |final class $ExpressionClassName
        |  extends (($contextType) => $resultOrSetterType) with $CompilerPkg.TemplateInterpolations[$resultType] {
        |    def apply($ContextSymbol: $contextType): $resultOrSetterType = {
        |    val $RootSymbol = $ContextSymbol.root
        |    val $VariablesSymbol = new com.avsystem.scex.util.DynamicVariableAccessor($ContextSymbol)
        |    import $profileObjectPkg.$ProfileObjectName._
        |    import Utils._
        |    import $RootSymbol._
        |    $rootGetterAdapterCode
        |    $profileHeader
        |    $additionalHeader
        |    val _result =
        |      $setterConversion(
        |      $MacroProcessor.processExpression[$contextType, $resultType](
        |      $MacroProcessor.applyTypesafeEquals({
        |""".stripMargin + interpolationPrefix

    val postfix = interpolationPostfix +
      s"""
        |     })))
        |    _result
        |  }
        |}
        |
      """.stripMargin

    val exprCode = prefix + expression + postfix
    val exprOffset = prefix.length

    (exprCode, exprOffset)
  }

  def generateProfileObject(profile: ExpressionProfile) = {
    val adapters = profile.symbolValidator.referencedJavaClasses.flatMap { clazz =>
      generateJavaGetterAdapter(clazz, full = false).toList.map { adapterCode =>
        val ExistentialType(polyTpe, typeVariables) = classToExistential(clazz)
        val wrappedTpe = javaTypeAsScalaType(polyTpe)
        val adapter = adapterName(clazz)
        val generics = if (typeVariables.nonEmpty) typeVariableDeclarations(typeVariables).mkString("[", ", ", "]") else ""
        val adapterWithGenerics = adapter + generics

        s"""
          |@$AnnotationPkg.JavaGetterAdapterConversion
          |implicit def $adapterWithGenerics(wrapped: $wrappedTpe) = new $adapter(wrapped)
          |$adapterCode
        """.stripMargin
      }
    }

    s"""
      |@$AnnotationPkg.ProfileObject
      |object $ProfileObjectName {
      |${adapters.mkString}
      |  @$AnnotationPkg.ExpressionUtil object Utils {
      |${profile.expressionUtils}
      |  }
      |}
      |
    """.stripMargin
  }

  def generateSyntaxValidator(code: String) = {
    s"""
      |final class $SyntaxValidatorClassName extends com.avsystem.scex.validation.SyntaxValidator {
      |$code
      |}
    """.stripMargin
  }

  def generateSymbolValidator(accessSpecs: String) = {
    s"""
      |final class $SymbolValidatorClassName extends com.avsystem.scex.validation.SymbolValidator {
      |  val accessSpecs = {$accessSpecs}
      |}
    """.stripMargin
  }

  def wrapInSource(code: String, pkgName: String): String = {
    s"""
      |package $pkgName
      |
      |$code
    """.stripMargin
  }

  def wrapInSource(code: String, offset: Int, pkgName: String): (String, Int) = {
    val prefix =
      s"""
      |package $pkgName
      |
      |""".stripMargin

    (prefix + code, prefix.length + offset)
  }

}
