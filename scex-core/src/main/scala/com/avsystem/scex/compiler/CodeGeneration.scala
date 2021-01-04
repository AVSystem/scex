package com.avsystem.scex
package compiler

import java.lang.reflect.{Method, Modifier}
import java.{lang => jl, util => ju}

import com.avsystem.scex.compiler.JavaTypeParsing._
import com.avsystem.scex.util.CommonUtils._

import scala.annotation.switch
import scala.language.existentials
import scala.reflect.NameTransformer

object CodeGeneration {

  // extractor object for java getters
  private object JavaGetter {
    def unapply(method: Method): Option[(String, Boolean)] =
      if (method.getParameterTypes.isEmpty && method.getTypeParameters.isEmpty) {

        def uncapitalize(str: String) =
          str.head.toLower.toString + str.tail
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

  val AdaptersPkgPrefix = "_scex_adapter_"
  val ProfilePkgPrefix = "_scex_profile_"
  val UtilsPkgPrefix = "_scex_utils_"
  val ExpressionPkgPrefix = "_scex_expr_"
  val SyntaxValidatorPkgPrefix = "_syntax_validator_"
  val SymbolValidatorPkgPrefix = "_symbol_validator_"
  val SymbolAttributesPkgPrefix = "_symbol_attributes_"
  val ConversionSupplierPkgPrefix = "_conversion_supplier_"
  val ArbitraryClassSourceNamePrefix = "_scex_class_"

  val VariableAccessorClassName = "_variableAccessor"
  val ExpressionClassName = "Expression"
  val ProfileObjectName = "Profile"
  val UtilsObjectName = "Utils"
  val SyntaxValidatorClassName = "SyntaxValidator"
  val SymbolValidatorClassName = "SymbolValidator"
  val SymbolAttributesClassName = "SymbolAttributes"
  val ConversionSupplierClassName = "ConversionSupplier"
  val ContextSymbol = "_ctx"
  val ImplicitContextSymbol = "_implicit_ctx"
  val VariablesSymbol = "_vars"
  val RootSymbol = "_root"
  val AdapterWrappedSymbol = "_wrapped"
  val AdaptedRootSymbol = "_adapted_root"
  val ScexPkg = "com.avsystem.scex"
  val CompilerPkg = s"$ScexPkg.compiler"
  val AnnotationPkg = s"$CompilerPkg.annotation"
  val MarkersObj = s"$CompilerPkg.Markers"
  val MacroProcessor = s"$CompilerPkg.ExpressionMacroProcessor"
  val InterpolationOpen = "t\"\"\""
  val NoMacrosInterpolationOpen = "p\"\"\""
  val InterpolationClose = "\"\"\""

  def adapterName(clazz: Class[_], full: Boolean) =
    (if (full) "Full" else "") + "Adapter_" + clazz.getName.replaceAll("\\.", "_")

  @switch def escapedChar(ch: Char): String = ch match {
    case '\b' => "\\b"
    case '\t' => "\\t"
    case '\n' => "\\n"
    case '\f' => "\\f"
    case '\r' => "\\r"
    case '"' => "\\\""
    case '\'' => "\\\'"
    case '\\' => "\\\\"
    case _ => if (ch.isControl) "\\0" + Integer.toOctalString(ch.toInt) else String.valueOf(ch)
  }

  def escapeString(str: String) =
    str.flatMap(escapedChar)

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
      case method@JavaGetter(propName, _) if !method.isSynthetic &&
        Modifier.isPublic(method.getModifiers) && !Modifier.isStatic(method.getModifiers) =>
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
           |class $adapterWithGenerics(private val $AdapterWrappedSymbol: $wrappedTpe)
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
    profileObjectPkg: Option[String],
    utilsObjectPkg: Option[String],
    noMacroProcessing: Boolean) = {

    val ExpressionDef(profile, template, setter, expression, header, contextType, resultType, variableTypes) = exprDef

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
         |      $MacroProcessor.validate[$contextType, ${if (setter) "Any" else resultType}](
         |      $MacroProcessor.applyTypesafeEquals(
      """.stripMargin

    val processingPostfix = if (noMacroProcessing) "" else "))))"

    val variableAccessorConstr =
      if (variableTypes.nonEmpty) VariableAccessorClassName
      else s"$ScexPkg.util.DynamicVariableAccessor[$ContextSymbol.type, $ContextSymbol.Var]($ContextSymbol)"

    val variableAccessorClassDef = if (variableTypes.nonEmpty) {
      val validatePrefix = if (noMacroProcessing) "" else s"$MacroProcessor.validate("
      val validatePostfix = if (noMacroProcessing) "" else ")"

      val typedVariables = variableTypes.toList.sorted.iterator.map {
        case (name, tpe) =>
          s"""
             |  private def ${name}VarTag = ${validatePrefix}inferVarTag[$tpe]$validatePostfix
             |
             |  @$AnnotationPkg.NotValidated def `$name`: $tpe =
             |    $ContextSymbol.getTypedVariable("${escapeString(name)}")(${name}VarTag)
             |
             |  @$AnnotationPkg.NotValidated def `${name}_=`(value: $tpe): Unit =
             |    $ContextSymbol.setTypedVariable("${escapeString(name)}", value)(${name}VarTag)
             |
         """.stripMargin
      }.mkString("\n")

      s"""
         |class $VariableAccessorClassName extends
         |  $ScexPkg.util.DynamicVariableAccessor[$ContextSymbol.type, $ContextSymbol.Var]($ContextSymbol) {
         |$typedVariables
         |}
       """.stripMargin
    } else ""

    val profileImport = profileObjectPkg.fold("")(pkg => s"import $pkg.$ProfileObjectName._")
    val utilsImport = utilsObjectPkg.fold("")(pkg => s"import $pkg.$UtilsObjectName._")

    //comment with profile name ensures that caching distinguishes between profiles
    //_result is needed because: https://groups.google.com/forum/#!topic/scala-user/BAK-mU7o6nM
    val prefix =
    s"""
       |// profile: ${profile.name}
       |
       |import scala.Predef.{wrapString => _, _}
       |
       |final class $ExpressionClassName(
       |  val debugInfo: com.avsystem.scex.ExpressionDebugInfo,
       |  val sourceInfo: com.avsystem.scex.compiler.SourceInfo
       |) extends $ScexPkg.AbstractExpression[$contextType, $resultOrSetterType]
       |  with $CompilerPkg.TemplateInterpolations[$resultType] {
       |
       |  def eval($ContextSymbol: $contextType @$AnnotationPkg.Input): $resultOrSetterType = {
       |    implicit def $ImplicitContextSymbol: $contextType @$AnnotationPkg.Input = $ContextSymbol
       |    val $RootSymbol = $ContextSymbol.root: @$AnnotationPkg.RootValue
       |    $profileImport
       |    $utilsImport
       |    import $RootSymbol._
       |    $rootGetterAdapterCode
       |    $profileHeader
       |    $additionalHeader
       |    $variableAccessorClassDef
       |    val $VariablesSymbol = new $variableAccessorConstr: @$AnnotationPkg.Input
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
         |implicit def $adapterWithGenerics(_wrapped: $wrappedTpe) =
         |  new $AdaptersPkgPrefix${NameTransformer.encode(profile.name)}.$adapterName(_wrapped)
        """.stripMargin
    }.mkString

    if (adapterConversions.nonEmpty) Some(
      s"""
         |object $ProfileObjectName extends $MarkersObj.ProfileObject {
         |$adapterConversions
         |}
         |
      """.stripMargin
    ) else None
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

  def implicitLiteralConversionClass(profileObjectPkg: Option[String], utilsObjectPkg: Option[String], profileHeader: String, header: String, resultType: String) = {
    val templateOptimizingScexCompiler = s"$ScexPkg.compiler.TemplateOptimizingScexCompiler"
    val profileImport = profileObjectPkg.fold("")(pkg => s"import $pkg.$ProfileObjectName._")
    val utilsImport = utilsObjectPkg.fold("")(pkg => s"import $pkg.$UtilsObjectName._")
    s"""
       |import scala.Predef.{wrapString => _, _}
       |
       |final class $ConversionSupplierClassName extends $templateOptimizingScexCompiler.ConversionSupplier[$resultType] {
       |  def get = {
       |    $profileImport
       |    $utilsImport
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
