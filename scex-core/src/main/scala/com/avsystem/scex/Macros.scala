package com.avsystem.scex

import java.{lang => jl, util => ju}

import com.avsystem.scex.compiler.TemplateInterpolations
import com.avsystem.scex.compiler.TemplateInterpolations.Splicer
import com.avsystem.scex.util.{MacroUtils, Literal => ScexLiteral}

import scala.reflect.macros.whitebox

/**
 * Created: 18-11-2013
 * Author: ghik
 */
object Macros {
  def templateInterpolation_impl[T: c.WeakTypeTag, A](c: whitebox.Context)(args: c.Expr[A]*): c.Expr[T] = {
    import c.universe._

    val Apply(_, List(Apply(_, parts))) = c.prefix.tree
    val argTrees = args.iterator.map(_.tree).toList

    def isStringLiteral(tree: Tree) = tree match {
      case Literal(Constant(str: String)) => true
      case _ => false
    }

    assert(parts forall isStringLiteral)
    assert(parts.size == args.size + 1)

    val resultType = weakTypeOf[T]
    val templateInterpolationsObjectTpe = typeOf[TemplateInterpolations.type]
    val templateInterpolationsObjectTree = reify(TemplateInterpolations).tree
    val splicerSymbol = typeOf[Splicer[_]].typeSymbol

    def reifyConcatenation(parts: List[Tree], args: List[Tree]) = {
      val convertedArgs = args.map { arg =>
        val splicerTpe = internal.reificationSupport.TypeRef(templateInterpolationsObjectTpe, splicerSymbol, List(arg.tpe))
        c.inferImplicitValue(splicerTpe) match {
          case EmptyTree => Select(arg, TermName("toString"))
          case tree => Apply(Select(tree, TermName("toString")), List(arg))
        }
      }

      Apply(Apply(Select(templateInterpolationsObjectTree, TermName("concat")), parts), convertedArgs)
    }

    def isEmptyStringLiteral(tree: Tree) = tree match {
      case Literal(Constant(str: String)) if str.isEmpty => true
      case _ => false
    }

    lazy val singleArgNoParts = args.size == 1 && parts.forall(isEmptyStringLiteral)
    lazy val soleArgTree = args.head.tree
    lazy val soleArgImplicitConv = c.inferImplicitView(soleArgTree, soleArgTree.tpe, resultType)

    // special cases for Java enums as there is no way to create general implicit conversion to arbitrary java enum
    // due to https://issues.scala-lang.org/browse/SI-7609
    if (resultType <:< typeOf[jl.Enum[_]] && args.size == 0) {
      val enumModuleSymbol = resultType.typeSymbol.companion
      val Literal(Constant(stringLiteral: String)) = parts.head

      c.Expr[T](Select(Ident(enumModuleSymbol), TermName(stringLiteral)))

    } else if (resultType <:< typeOf[jl.Enum[_]] && singleArgNoParts && !(soleArgTree.tpe <:< resultType)) {
      val enumModuleSymbol = resultType.typeSymbol.companion

      c.Expr[T](Apply(Select(Ident(enumModuleSymbol), TermName("valueOf")), List(args.head.tree)))

    } else if (singleArgNoParts) {
      // typecheck result manually
      if (soleArgTree.tpe <:< resultType) {
        c.Expr[T](soleArgTree)
      } else if (soleArgImplicitConv != EmptyTree) {
        c.Expr[T](Apply(soleArgImplicitConv, List(soleArgTree)))
      } else if (typeOf[String] <:< resultType) {
        c.Expr[T](reifyConcatenation(parts, argTrees))
      } else {
        c.error(soleArgTree.pos, s"This template (type ${soleArgTree.tpe.widen}) cannot represent value of type $resultType")
        null
      }

    } else if (args.size == 0) {
      val Literal(Constant(literalString: String)) = parts.head
      val literalExpr = reify(com.avsystem.scex.util.Literal(c.literal(literalString).splice))
      c.inferImplicitView(literalExpr.tree, typeOf[com.avsystem.scex.util.Literal], resultType) match {
        case EmptyTree =>
          c.error(parts.head.pos, s"""String literal "$literalString" cannot be parsed as value of type $resultType""")
          null

        case conversion =>
          c.Expr[T](Apply(conversion, List(literalExpr.tree)))
      }

    } else if (typeOf[String] <:< resultType) {
      c.Expr[T](reifyConcatenation(parts, argTrees))
    } else {
      c.error(c.enclosingPosition, s"This template cannot represent value of type $resultType")
      null
    }
  }

  def reifyImplicitView_impl[T: c.WeakTypeTag](c: whitebox.Context)(arg: c.Expr[Any]): c.Expr[T] = {
    import c.universe._

    val fromType = arg.actualType
    val toType = weakTypeOf[T]

    val view = c.inferImplicitView(arg.tree, fromType, toType, silent = false, withMacrosDisabled = true)
    c.Expr[T](Apply(view, List(arg.tree)))
  }

  def checkConstantExpr_impl[T](c: whitebox.Context)(expr: c.Expr[T]): c.Expr[T] = {
    val utils = MacroUtils(c.universe)
    import utils._

    expr.tree.foreach { t =>
      if (isAnnotatedWith(t.tpe.widen, inputAnnotType)) {
        c.error(t.pos, s"Tree references expression input")
      }
    }

    expr
  }

  def tripleEquals_impl[A, B](c: whitebox.Context)(right: c.Expr[B]): c.Expr[Boolean] = {
    import c.universe._

    val Apply(_, List(leftTree)) = c.prefix.tree
    val rightTree = right.tree

    val leftTpe = leftTree.tpe.widen
    val rightTpe = rightTree.tpe.widen

    lazy val leftToRightConv = c.inferImplicitView(leftTree, leftTree.tpe, rightTree.tpe.widen)
    lazy val rightToLeftConv = c.inferImplicitView(rightTree, rightTree.tpe, leftTree.tpe.widen)

    if (leftTpe <:< rightTpe) {
      reify(c.Expr[Any](leftTree).splice == c.Expr[Any](rightTree).splice)
    } else if (rightTpe <:< leftTpe) {
      reify(c.Expr[Any](rightTree).splice == c.Expr[Any](leftTree).splice)
    } else if (rightToLeftConv != EmptyTree) {
      reify(c.Expr[Any](leftTree).splice == c.Expr[Any](Apply(rightToLeftConv, List(rightTree))).splice)
    } else if (leftToRightConv != EmptyTree) {
      reify(c.Expr[Any](Apply(leftToRightConv, List(leftTree))).splice == c.Expr[Any](rightTree).splice)
    } else {
      c.error(c.enclosingPosition, s"Values of types $leftTpe and $rightTpe cannot be compared for equality")
      null
    }
  }

}
