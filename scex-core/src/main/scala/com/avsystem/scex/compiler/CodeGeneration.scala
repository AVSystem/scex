package com.avsystem.scex
package compiler

import java.lang.reflect.{Method, Modifier}
import java.{lang => jl, util => ju}

import com.avsystem.scex.compiler.JavaTypeParsing._
import com.avsystem.scex.util.CommonUtils._

import scala.language.existentials

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

  val AdaptersPkg = "_scex_adapter"
  val ProfilePkgPrefix = "_scex_profile_"
  val UtilsPkgPrefix = "_scex_utils_"
  val ExpressionPkgPrefix = "_scex_expr_"
  val SyntaxValidatorPkgPrefix = "_syntax_validator_"
  val SymbolValidatorPkgPrefix = "_symbol_validator_"
  val SymbolAttributesPkgPrefix = "_symbol_attributes_"
  val ConversionSupplierPkgPrefix = "_conversion_supplier_"
  val ArbitraryClassSourceNamePrefix = "_scex_class_"

  val ExpressionClassName = "Expression"
  val ProfileObjectName = "Profile"
  val UtilsObjectName = "Utils"
  val SyntaxValidatorClassName = "SyntaxValidator"
  val SymbolValidatorClassName = "SymbolValidator"
  val SymbolAttributesClassName = "SymbolAttributes"
  val ConversionSupplierClassName = "ConversionSupplier"
  val ContextSymbol = "_ctx"
  val VariablesSymbol = "_vars"
  val RootSymbol = "_root"
  val AdaptedRootSymbol = "_adapted_root"
  val ScexPkg = "com.avsystem.scex"
  val CompilerPkg = s"$ScexPkg.compiler"
  val AnnotationPkg = s"$CompilerPkg.annotation"
  val MarkersObj = s"$CompilerPkg.Markers"
  val MacroProcessor = s"$CompilerPkg.ExpressionMacroProcessor"
  val InterpolationOpen = "t\"\"\""
  val NoMacrosInterpolationOpen = "s\"\"\""
  val InterpolationClose = "\"\"\""

  def adapterName(clazz: Class[_], full: Boolean) =
    (if (full) "Full" else "") + "Adapter_" + clazz.getName.replaceAll("\\.", "_")

  /**
   * Generates code of implicit view for given Java class that adds Scala-style getters
   * forwarding to existing Java-style getters of given class.
   */
  def generateJavaGetterAdapter(clazz: Class[_], full: Boolean): Option[String] = {
    // generate scala getters
    val methods =
      if (full) clazz.getMethods
      else clazz.getMethods.filter(m => m.getDeclaringClass == clazz || isMultipleInherited(clazz, m))

    val scalaGetters = methods.collect {
      case method@JavaGetter(propName, booleanIsGetter) if Modifier.isPublic(method.getModifiers) =>
        s"def `$propName` = _wrapped.${method.getName}\n"
    }

    if (full || scalaGetters.nonEmpty) {

      val classBody = scalaGetters.sorted.mkString

      val ExistentialType(polyTpe, typeVariables) = classToExistential(clazz)
      val generics = if (typeVariables.nonEmpty) typeVariableDeclarations(typeVariables).mkString("[", ", ", "]") else ""
      val wrappedTpe = javaTypeAsScalaType(polyTpe)
      val adapterWithGenerics = adapterName(clazz, full) + generics

      val result =
        s"""
        |class $adapterWithGenerics(private val _wrapped: $wrappedTpe)
        |  extends AnyVal with $MarkersObj.JavaGetterAdapter {
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
    profileObjectPkg: String,
    utilsObjectPkg: String,
    noMacroProcessing: Boolean) = {

    val ExpressionDef(profile, template, setter, expression, header, contextType, resultType) = exprDef

    val resultOrSetterType = if (setter) s"$ScexPkg.Setter[$resultType]" else resultType

    val profileHeader = Option(profile.expressionHeader).getOrElse("")
    val additionalHeader = Option(header).getOrElse("")

    val rootGetterAdapterCode = fullAdapterClassNameOpt match {
      case Some(fullAdapterClassName) =>
        s"""
        |val $AdaptedRootSymbol = new $fullAdapterClassName($RootSymbol): @$AnnotationPkg.RootAdapter
        |import $AdaptedRootSymbol._
        |""".stripMargin
      case None =>
        ""
    }

    val interpolationPrefix = if (template) if (!noMacroProcessing) InterpolationOpen else NoMacrosInterpolationOpen else ""
    val interpolationPostfix = if (template) InterpolationClose else ""
    val setterConversion = if (setter) s"$MacroProcessor.asSetter[$resultType]" else ""

    val processingPrefix = if (noMacroProcessing) ""
    else
      s"""
        |      $MacroProcessor.markExpression(
        |      $setterConversion(
        |      $MacroProcessor.processExpression[$contextType, ${if (setter) "Any" else resultType}](
        |      $MacroProcessor.applyTypesafeEquals(
      """.stripMargin

    val processingPostfix = if (noMacroProcessing) "" else "))))"

    //_result is needed because: https://groups.google.com/forum/#!topic/scala-user/BAK-mU7o6nM
    val prefix =
      s"""
        |
        |final class $ExpressionClassName(
        |  val debugInfo: com.avsystem.scex.ExpressionDebugInfo,
        |  val sourceInfo: com.avsystem.scex.compiler.SourceInfo)
        |
        |  extends $ScexPkg.AbstractExpression[$contextType, $resultOrSetterType]
        |  with $CompilerPkg.TemplateInterpolations[$resultType] {
        |
        |  def eval($ContextSymbol: $contextType @$AnnotationPkg.Input): $resultOrSetterType = {
        |    val $RootSymbol = $ContextSymbol.root: @$AnnotationPkg.RootValue
        |    val $VariablesSymbol = new $ScexPkg.util.DynamicVariableAccessor($ContextSymbol): @$AnnotationPkg.Input
        |    import $profileObjectPkg.$ProfileObjectName._
        |    import $utilsObjectPkg.$UtilsObjectName._
        |    import $RootSymbol._
        |    $rootGetterAdapterCode
        |    $profileHeader
        |    $additionalHeader
        |    val _result =
        |      $processingPrefix
        |      {
        |""".stripMargin + interpolationPrefix

    val postfix = interpolationPostfix +
      s"""
        |    }
        |    $processingPostfix
        |    _result
        |  }
        |}
        |
      """.stripMargin

    val exprCode = prefix + expression + postfix
    val exprOffset = prefix.length

    (exprCode, exprOffset)
  }

  def generateProfileObject(profile: ExpressionProfile, adapters: Seq[(Class[_], String)]) = {
    val adapterConversions = adapters.iterator.map { case (clazz, adapterName) =>
      val ExistentialType(polyTpe, typeVariables) = classToExistential(clazz)
      val wrappedTpe = javaTypeAsScalaType(polyTpe)
      val generics = if (typeVariables.nonEmpty) typeVariableDeclarations(typeVariables).mkString("[", ", ", "]") else ""
      val adapterWithGenerics = adapterName + generics

      s"""
          |implicit def $adapterWithGenerics(_wrapped: $wrappedTpe) = new $AdaptersPkg.$adapterName(_wrapped)
        """.stripMargin
    }.mkString

    s"""
      |object $ProfileObjectName extends $MarkersObj.ProfileObject {
      |$adapterConversions
      |}
      |
    """.stripMargin
  }

  def generateExpressionUtils(code: String) = {
    s"""
       |object Utils extends $MarkersObj.ExpressionUtil {
       |$code
       |}
     """.stripMargin
  }

  def wrapForParsing(code: String, template: Boolean): (String, Int) = {
    val prefix = "object o {" + (if (template) InterpolationOpen else "")
    val postfix = (if (template) InterpolationClose else "") + "}"
    (s"$prefix$code$postfix", prefix.length)
  }

  def generateSyntaxValidator(code: String) = {
    s"""
      |final class $SyntaxValidatorClassName extends $ScexPkg.validation.SyntaxValidator {
      |$code
      |}
    """.stripMargin
  }

  def generateSymbolValidator(accessSpecs: String) = {
    s"""
      |final class $SymbolValidatorClassName extends $ScexPkg.validation.SymbolValidator {
      |  val infoList = {$accessSpecs}
      |}
    """.stripMargin
  }

  def generateSymbolAttributes(attributes: String) = {
    s"""
      |final class $SymbolAttributesClassName extends $ScexPkg.presentation.SymbolAttributes({$attributes})
    """.stripMargin
  }

  def wrapInSource(code: String, pkgName: String): String = {
    s"""
      |package $pkgName
      |
      |$code
    """.stripMargin
  }

  def wrapInSource(code: String, offset: Int, pkgName: String): (String, String, Int) = {
    val prefix =
      s"""
      |package $pkgName
      |
      |""".stripMargin

    (pkgName, prefix + code, prefix.length + offset)
  }

  def implicitLiteralViewHeader(header: String) =
    s"$header; val _dummy_literal: $ScexPkg.util.Literal = ???\n"

  def implicitLiteralViewExpression(resultType: String) = {
    val templateOptimizingObj = s"$ScexPkg.compiler.TemplateOptimizingScexCompiler"
    s"$templateOptimizingObj.checkConstant($templateOptimizingObj.reifyImplicitView[$resultType](_dummy_literal))\n"
  }

  def implicitLiteralConversionClass(profileObjectPkg: String, utilsObjectPkg: String, profileHeader: String, header: String, resultType: String) = {
    val templateOptimizingScexCompiler = s"$ScexPkg.compiler.TemplateOptimizingScexCompiler"
    s"""
      |final class $ConversionSupplierClassName extends $templateOptimizingScexCompiler.ConversionSupplier[$resultType] {
      |  def get = {
      |    import $profileObjectPkg.$ProfileObjectName._
      |    import $utilsObjectPkg.$UtilsObjectName._
      |    $profileHeader
      |    $header
      |    implicitly[$ScexPkg.util.Literal => ($resultType)]
      |  }
      |
      |  def isNullable = $templateOptimizingScexCompiler.isNullable[$resultType]
      |}
    """.stripMargin
  }

}
