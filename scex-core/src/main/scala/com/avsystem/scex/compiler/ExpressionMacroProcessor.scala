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
object ExpressionMacroProcessor {
  def markExpression[T](expr: T): T = macro ExpressionMacroProcessor.markExpression_impl[T]

  def processExpression[C, T](expr: T): T = macro ExpressionMacroProcessor.processExpression_impl[C, T]

  def applyTypesafeEquals[T](expr: T): T = macro ExpressionMacroProcessor.applyTypesafeEquals_impl[T]

  def asSetter[T](expr: Any): Setter[T] = macro ExpressionMacroProcessor.asSetter_impl[T]
}

class ExpressionMacroProcessor(val c: whitebox.Context) extends MacroUtils with LoggingUtils {
  val universe: c.universe.type = c.universe
  import universe._

  private val logger = createLogger[ExpressionMacroProcessor]

  def markExpression_impl[T](expr: c.Expr[T]): c.Tree = {
    c.internal.updateAttachment(expr.tree, ExpressionTreeAttachment)
    expr.tree
  }

  def processExpression_impl[C: c.WeakTypeTag, T](expr: c.Expr[T]): c.Tree = {
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

    expr.tree
  }

  def applyTypesafeEquals_impl[T](expr: c.Expr[T]): c.Tree = {
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
      if (transformed) c.untypecheck(result) else result

    } else expr.tree

  }

  def asSetter_impl[T: c.WeakTypeTag](expr: c.Expr[Any]): c.Tree = {
    lazy val ttpe = weakTypeOf[T]

    def reifySetterFunction(setter: Tree) =
      reifyFunction(arg => q"$setter($arg)")

    def reifyFunction(bodyGen: Tree => Tree): Tree =
      q"(value: $ttpe) => ${bodyGen(q"value")}"

    def translate(tree: Tree): Tree = tree match {
      case Select(prefix@Ident(_), TermName(propertyName)) if isRootAdapter(prefix.tpe) =>
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
        reifyFunction(arg => q"$tree = $arg")

      case Apply(fun, Nil) =>
        translate(fun)

      case Apply(Select(prefix, TermName("apply")), List(soleArgument)) =>
        reifyFunction(arg => q"$prefix.update($soleArgument, $arg)")

      case Apply(Select(prefix, TermName("selectDynamic")), List(dynamicNameArg))
        if prefix.tpe <:< typeOf[Dynamic] =>

        reifySetterFunction(q"$prefix.updateDynamic($dynamicNameArg)")

      case Typed(inner, _) =>
        translate(inner)

      case _ =>
        c.abort(tree.pos, "Cannot translate this expression into setter")
    }

    val TypeObj = typeOf[Type.type].termSymbol
    q"""
       new ${weakTypeOf[Setter[T]]} {
         def apply(value: ${weakTypeOf[T]}) = ${translate(expr.tree)}(value)

         def acceptedType = $TypeObj(${expr.actualType.map(_.widen).toString}, ${c.reifyRuntimeClass(expr.actualType)})
       }
     """
  }
}
