package com.avsystem.scex

import com.avsystem.scex.util.{Literal => ScexLiteral, MacroUtils}
import java.{util => ju, lang => jl}
import scala.reflect.macros.whitebox.Context
import scala.util.control.NonFatal
import com.avsystem.scex.compiler.{CodeGeneration, TemplateInterpolations}
import com.avsystem.scex.compiler.TemplateInterpolations.Splicer

/**
 * Created: 18-11-2013
 * Author: ghik
 */
object Macros {
  def templateInterpolation_impl[T: c.WeakTypeTag](c: Context)(args: c.Expr[Any]*): c.Expr[T] = {
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
        c.inferImplicitValue(internal.reificationSupport.TypeRef(templateInterpolationsObjectTpe, splicerSymbol, List(arg.tpe))) match {
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

    if (typeOf[String] <:< resultType) {
      c.Expr[T](reifyConcatenation(parts, argTrees))
    }
    // special cases for Java enums as there is no way to create general implicit conversion to arbitrary java enum
    // due to https://issues.scala-lang.org/browse/SI-7609
    else if (resultType <:< typeOf[jl.Enum[_]] && args.size == 0) {
      val enumModuleSymbol = resultType.typeSymbol.companion
      val Literal(Constant(stringLiteral: String)) = parts.head

      c.Expr[T](Select(Ident(enumModuleSymbol), TermName(stringLiteral)))

    } else if (resultType <:< typeOf[jl.Enum[_]] && args.size == 1 && parts.forall(isEmptyStringLiteral) && !(args.head.actualType <:< resultType)) {
      val enumModuleSymbol = resultType.typeSymbol.companion

      c.Expr[T](Apply(Select(Ident(enumModuleSymbol), TermName("valueOf")), List(args.head.tree)))

    } else if (args.size == 1 && parts.forall(isEmptyStringLiteral)) {
      val soleTree = args.head.tree
      lazy val implicitConv = c.inferImplicitView(soleTree, soleTree.tpe, resultType)

      // typecheck result manually
      if (soleTree.tpe <:< resultType) {
        c.Expr[T](soleTree)
      } else if (implicitConv != EmptyTree) {
        c.Expr[T](Apply(implicitConv, List(soleTree)))
      } else {
        c.error(soleTree.pos, s"This template (type ${soleTree.tpe.widen}) cannot represent value of type $resultType")
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

    } else {
      c.error(c.enclosingPosition, s"This template cannot represent value of type $resultType")
      null
    }
  }

  def reifyImplicitView_impl[T: c.WeakTypeTag](c: Context)(arg: c.Expr[Any]): c.Expr[T] = {
    import c.universe._

    val fromType = arg.actualType
    val toType = weakTypeOf[T]

    val view = c.inferImplicitView(arg.tree, fromType, toType, silent = false, withMacrosDisabled = true)
    c.Expr[T](Apply(view, List(arg.tree)))
  }

  def checkConstantExpr_impl[T](c: Context)(expr: c.Expr[T]): c.Expr[T] = {
    import c.universe._
    val macroUtils = MacroUtils(c.universe)
    import macroUtils._

    import CodeGeneration._

    lazy val inputSymbols = Set(ContextSymbol, RootSymbol, VariablesSymbol)

    expr.tree.foreach {
      case tree@Ident(TermName(name)) if inputSymbols.contains(name) =>
        c.error(tree.pos, s"Tree references expression input")
      case _ =>
    }

    expr
  }

  def tripleEquals_impl[A, B](c: Context)(right: c.Expr[B]): c.Expr[Boolean] = {
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
