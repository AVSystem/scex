package com.avsystem.scex
package compiler

import com.avsystem.scex.util.{LoggingUtils, CommonUtils, MacroUtils, TypesafeEquals}
import com.avsystem.scex.validation.ValidationContext
import java.{util => ju, lang => jl}
import scala.language.experimental.macros
import scala.reflect.macros.Context

/**
 * Object used during expression compilation to validate the expression (syntax, invocations, etc.)
 * This must be a Scala object and not a class because it contains macros. Validation is performed against
 * given ExpressionProfile which is injected into this object by ScexCompiler by means of a dynamic variable.
 */
object ExpressionMacroProcessor extends LoggingUtils {

  private val logger = createLogger[ExpressionMacroProcessor.type]

  def processExpression[C, T](expr: T): T = macro processExpression_impl[C, T]

  def processExpression_impl[C: c.WeakTypeTag, T](c: Context)(expr: c.Expr[T]): c.Expr[T] = {
    import c.universe._
    val validationContext = ValidationContext(c.universe)(weakTypeOf[C])
    import validationContext._

    val profile = c.enclosingUnit.source match {
      case scexSource: ExpressionSourceFile => scexSource.profile
      case _ => throw new Exception("This is not an expression source file")
    }

    def isForbiddenThisReference(tree: Tree) = tree match {
      case tree: This if !tree.symbol.isPackage && !tree.symbol.isPackageClass => true
      case _ => false
    }

    expr.tree.foreach { subtree =>
      if (isForbiddenThisReference(subtree)) {
        c.error(subtree.pos, s"Cannot refer to 'this' or outer instances")
      }
      if (!profile.syntaxValidator.isSyntaxAllowed(c.universe)(subtree)) {
        c.error(subtree.pos, s"Cannot use language construct: ${subtree.getClass.getSimpleName}")
      }
    }

    val access = extractAccess(expr.tree)
    logger.trace(s"Validating expression member access:\n${access.repr}")

    val validationResult = profile.symbolValidator.validateMemberAccess(validationContext)(access)

    validationResult.deniedAccesses.foreach { access =>
      c.error(access.pos, s"Member access forbidden: $access")
    }

    expr
  }

  def applyTypesafeEquals[T](expr: T): T = macro applyTypesafeEquals_impl[T]

  def applyTypesafeEquals_impl[T](c: Context)(expr: c.Expr[T]): c.Expr[T] = {
    import c.universe._

    if (c.inferImplicitValue(typeOf[TypesafeEquals.TypesafeEqualsEnabled.type]) != EmptyTree) {
      var transformed = false

      def tripleEquals(left: Tree, right: Tree) = {
        transformed = true
        Apply(Select(left, newTermName("===").encodedName), List(right))
      }

      object transformer extends Transformer {
        override def transform(tree: Tree) = tree match {
          case Apply(Select(left, operator), List(right)) => operator.decoded match {
            case "==" => tripleEquals(transform(left), transform(right))
            case "!=" => Select(tripleEquals(transform(left), transform(right)), newTermName("unary_!").encodedName)
            case _ => super.transform(tree)
          }
          case _ => super.transform(tree)
        }
      }

      val result = transformer.transform(expr.tree)
      c.Expr[T](if (transformed) c.resetAllAttrs(result) else result)

    } else expr

  }

  def asSetter[T](expr: T): Setter[T] = macro asSetter_impl[T]

  def asSetter_impl[T: c.WeakTypeTag](c: Context)(expr: c.Expr[T]): c.Expr[Setter[T]] = {
    val macroUtils = MacroUtils(c.universe)

    import c.universe._
    import macroUtils._

    lazy val ttpe = weakTypeOf[T]

    def reifySetterFunction(setter: Tree) =
      reifyFunction(arg => Apply(setter, List(arg)))

    def reifyFunction(bodyGen: Tree => Tree): Tree =
      Function(List(ValDef(Modifiers(Flag.PARAM), newTermName("value"), TypeTree(ttpe), EmptyTree)),
        bodyGen(Ident(newTermName("value"))))

    def translate(tree: Tree): Tree = tree match {
      case Select(prefix@Ident(_), TermName(propertyName)) if prefix.symbol.annotations.exists(_.tpe <:< rootAdapterAnnotType) =>
        reifySetterFunction(Select(Ident(newTermName(CodeGeneration.RootSymbol)), newTermName("set" + propertyName.capitalize)))

      case Select(ImplicitlyConverted(prefix, fun), TermName(propertyName)) if isAdapterConversion(fun.symbol) =>
        reifySetterFunction(Select(prefix, newTermName("set" + propertyName.capitalize)))

      case Select(prefix, TermName(getterName)) if tree.symbol.isMethod =>
        import CommonUtils._

        val returnType = tree.symbol.asMethod.returnType
        val setterName = getterName match {
          case BooleanBeanGetterNamePattern(capitalizedProperty, _) if returnType <:< booleanTpe || returnType <:< jBooleanTpe =>
            "set" + capitalizedProperty
          case BeanGetterNamePattern(capitalizedProperty, _) =>
            "set" + capitalizedProperty
          case _ =>
            getterName + "_="
        }

        reifySetterFunction(Select(prefix, newTermName(setterName).encodedName))

      case Select(prefix, TermName(name)) if isJavaField(tree.symbol) =>
        reifyFunction(arg => Assign(tree, arg))

      case Apply(fun, Nil) =>
        translate(fun)

      case Apply(Select(prefix, TermName("apply")), List(soleArgument)) =>
        reifyFunction(arg => Apply(Select(prefix, newTermName("update")), List(soleArgument, arg)))

      case Apply(Select(prefix, TermName("selectDynamic")), List(dynamicNameArg))
        if prefix.tpe <:< typeOf[Dynamic] =>

        reifySetterFunction(Apply(Select(prefix, newTermName("updateDynamic")), List(dynamicNameArg)))

      case _ =>
        c.error(tree.pos, "Cannot translate this expression into setter")
        null
    }

    reify {
      new Setter[T] {
        def apply(value: T) = c.Expr[T => Unit](translate(expr.tree)).splice.apply(value)
      }
    }
  }
}
