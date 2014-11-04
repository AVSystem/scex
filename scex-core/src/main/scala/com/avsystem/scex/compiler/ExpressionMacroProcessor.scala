package com.avsystem.scex
package compiler

import java.{lang => jl, util => ju}

import com.avsystem.scex.util.{LoggingUtils, MacroUtils, TypesafeEquals}
import com.avsystem.scex.validation.ValidationContext

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

/**
 * Object used during expression compilation to validate the expression (syntax, invocations, etc.)
 * This must be a Scala object and not a class because it contains macros. Validation is performed against
 * given ExpressionProfile which is injected into this object by ScexCompiler by means of a dynamic variable.
 */
object ExpressionMacroProcessor extends LoggingUtils {

  private val logger = createLogger[ExpressionMacroProcessor.type]

  def processExpression[C, T](expr: T): T = macro processExpression_impl[C, T]

  def processExpression_impl[C: c.WeakTypeTag, T](c: whitebox.Context)(expr: c.Expr[T]): c.Expr[T] = {
    import c.universe._
    val validationContext = ValidationContext(c.universe)(weakTypeOf[C])
    import validationContext._

    val profile = expr.tree.pos.source match {
      case scexSource: ExpressionSourceFile => scexSource.exprDef.profile
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
    }

    def validateSyntax(trees: List[Tree]): Unit = trees match {
      case head :: tail =>
        val (allowed, children) = profile.syntaxValidator.validateSyntax(c.universe)(head)
        if (!allowed) {
          c.error(head.pos, s"Forbidden language construct: ${head.productPrefix}")
        }
        validateSyntax(children ::: tail)
      case Nil =>
    }

    validateSyntax(List(expr.tree))

    val access = extractAccess(expr.tree)
    logger.trace(s"Validating expression member access:\n${access.repr}")

    val validationResult = profile.symbolValidator.validateMemberAccess(validationContext)(access)

    validationResult.deniedAccesses.foreach { access =>
      c.error(access.pos, s"Member access forbidden: $access")
    }



    expr
  }

  def applyTypesafeEquals[T](expr: T): T = macro applyTypesafeEquals_impl[T]

  def applyTypesafeEquals_impl[T](c: whitebox.Context)(expr: c.Expr[T]): c.Expr[T] = {
    import c.universe._

    if (c.inferImplicitValue(typeOf[TypesafeEquals.TypesafeEqualsEnabled.type]) != EmptyTree) {
      var transformed = false

      object transformer extends Transformer {
        override def transform(tree: Tree) = tree match {
          case apply@Apply(select@Select(left, operator), List(right)) => operator.decodedName.toString match {
            case "==" =>
              transformed = true
              internal.setPos(Apply(
                internal.setPos(Select(transform(left), TermName("===").encodedName), select.pos),
                List(transform(right))
              ), apply.pos)
            case "!=" =>
              transformed = true
              internal.setPos(Select(
                internal.setPos(Apply(
                  internal.setPos(Select(transform(left), TermName("===").encodedName), select.pos),
                  List(transform(right))
                ), apply.pos),
                TermName("unary_!").encodedName
              ), apply.pos)
            case _ =>
              super.transform(tree)
          }
          case _ =>
            super.transform(tree)
        }
      }

      val result = transformer.transform(expr.tree)
      c.Expr[T](if (transformed) c.untypecheck(result) else result)

    } else expr

  }

  def asSetter[T](expr: T): Setter[T] = macro asSetter_impl[T]

  def asSetter_impl[T: c.WeakTypeTag](c: whitebox.Context)(expr: c.Expr[T]): c.Expr[Setter[T]] = {
    val macroUtils = MacroUtils(c.universe)

    import c.universe._
    import macroUtils._

    lazy val ttpe = weakTypeOf[T]

    def reifySetterFunction(setter: Tree) =
      reifyFunction(arg => Apply(setter, List(arg)))

    def reifyFunction(bodyGen: Tree => Tree): Tree =
      Function(List(ValDef(Modifiers(Flag.PARAM), TermName("value"), TypeTree(ttpe), EmptyTree)),
        bodyGen(Ident(TermName("value"))))

    def translate(tree: Tree): Tree = tree match {
      case Select(prefix@Ident(_), TermName(propertyName)) if isRootAdapter(prefix.tpe.widen) =>
        reifySetterFunction(Select(Ident(TermName(CodeGeneration.RootSymbol)), TermName("set" + propertyName.capitalize)))

      case Select(ImplicitlyConverted(prefix, fun), TermName(propertyName)) if isAdapterConversion(fun.symbol) =>
        reifySetterFunction(Select(prefix, TermName("set" + propertyName.capitalize)))

      case Select(prefix, TermName(getterName)) if tree.symbol.isMethod =>
        import com.avsystem.scex.util.CommonUtils._

        val returnType = tree.symbol.asMethod.returnType
        val setterName = getterName match {
          case BooleanBeanGetterNamePattern(capitalizedProperty, _) if returnType <:< booleanTpe || returnType <:< jBooleanTpe =>
            "set" + capitalizedProperty
          case BeanGetterNamePattern(capitalizedProperty, _) =>
            "set" + capitalizedProperty
          case _ =>
            getterName + "_="
        }

        reifySetterFunction(Select(prefix, TermName(setterName).encodedName))

      case Select(prefix, TermName(name)) if isJavaField(tree.symbol) =>
        reifyFunction(arg => Assign(tree, arg))

      case Apply(fun, Nil) =>
        translate(fun)

      case Apply(Select(prefix, TermName("apply")), List(soleArgument)) =>
        reifyFunction(arg => Apply(Select(prefix, TermName("update")), List(soleArgument, arg)))

      case Apply(Select(prefix, TermName("selectDynamic")), List(dynamicNameArg))
        if prefix.tpe <:< typeOf[Dynamic] =>

        reifySetterFunction(Apply(Select(prefix, TermName("updateDynamic")), List(dynamicNameArg)))

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
