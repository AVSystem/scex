package com.avsystem.scex

import java.{lang => jl, util => ju}

import com.avsystem.commons.macros.MacroCommons
import com.avsystem.scex.util.MacroUtils

import scala.reflect.macros.blackbox

/**
  * Created: 18-11-2013
  * Author: ghik
  */
class Macros(val c: blackbox.Context) extends MacroCommons with MacroUtils {

  lazy val universe: c.universe.type = c.universe

  import universe._

  lazy val ScexLiteralTpe = getType(tq"$ScexPkg.util.Literal")
  lazy val ScexLiteralObj = ScexLiteralTpe.typeSymbol.companion
  lazy val TemplateInterpolationsObj = q"$ScexPkg.compiler.TemplateInterpolations"

  def templateInterpolation_impl[T: c.WeakTypeTag, A](args: c.Expr[A]*): c.Tree = {
    val Apply(_, List(Apply(_, parts))) = c.prefix.tree
    val argTrees = args.iterator.map(_.tree).toList

    def isStringLiteral(tree: Tree) = tree match {
      case Literal(Constant(str: String)) => true
      case _ => false
    }

    assert(parts forall isStringLiteral)
    assert(parts.size == args.size + 1)

    val resultType = weakTypeOf[T]

    def reifyConcatenation(parts: List[Tree], args: List[Tree]) = {
      val convertedArgs = args.map { arg =>
        val splicerTpe = getType(tq"$TemplateInterpolationsObj.Splicer[${arg.tpe}]")
        c.inferImplicitValue(splicerTpe) match {
          case EmptyTree => q"$arg.toString"
          case tree => q"$tree.toString($arg)"
        }
      }

      q"$TemplateInterpolationsObj.concat(..$parts)(..$convertedArgs)"
    }

    def isEmptyStringLiteral(tree: Tree) = tree match {
      case Literal(Constant(str: String)) if str.isEmpty => true
      case _ => false
    }

    lazy val singleArgNoParts = args.size == 1 && parts.forall(isEmptyStringLiteral)
    lazy val soleArgTree = args.head.tree
    lazy val soleArgImplicitConv = c.inferImplicitView(soleArgTree, soleArgTree.tpe, resultType)

    lazy val Literal(Constant(literalString: String)) = parts.head
    lazy val literalTree = q"$ScexLiteralObj($literalString)"
    lazy val literalConv = c.inferImplicitView(literalTree, ScexLiteralTpe, resultType)

    if (args.isEmpty && isEmptyStringLiteral(parts.head) && typeOf[Null] <:< resultType) {
      q"null"
    } else if (resultType <:< typeOf[jl.Enum[_]] && args.isEmpty && literalConv == EmptyTree) {
      // special cases for Java enums as there is no way to create general implicit conversion to arbitrary java enum
      // due to https://issues.scala-lang.org/browse/SI-7609
      val enumModuleSymbol = resultType.typeSymbol.companion
      q"$enumModuleSymbol.${TermName(literalString)}"

    } else if (resultType <:< typeOf[jl.Enum[_]] && singleArgNoParts && !(soleArgTree.tpe <:< resultType)) {
      val enumModuleSymbol = resultType.typeSymbol.companion

      q"$enumModuleSymbol.valueOf(${args.head})"
    } else if (singleArgNoParts) {
      // typecheck result manually
      if (soleArgTree.tpe <:< resultType) {
        soleArgTree
      } else if (soleArgImplicitConv != EmptyTree) {
        q"$soleArgImplicitConv($soleArgTree)"
      } else if (typeOf[String] <:< resultType) {
        reifyConcatenation(parts, argTrees)
      } else {
        c.abort(soleArgTree.pos, s"This template (type ${soleArgTree.tpe.widen}) cannot represent value of type $resultType")
      }

    } else if (args.isEmpty) {
      literalConv match {
        case EmptyTree =>
          c.abort(parts.head.pos, s"""String literal "$literalString" cannot be parsed as value of type $resultType""")

        case conversion =>
          q"$conversion($literalTree)"
      }

    } else if (typeOf[String] <:< resultType) {
      reifyConcatenation(parts, argTrees)
    } else {
      c.abort(c.enclosingPosition, s"This template cannot represent value of type $resultType")
    }
  }

  def reifyImplicitView_impl[T: c.WeakTypeTag](arg: c.Expr[Any]): c.Tree = {
    val fromType = arg.actualType
    val toType = weakTypeOf[T]

    val view = c.inferImplicitView(arg.tree, fromType, toType, silent = false, withMacrosDisabled = true)
    q"$view($arg)"
  }

  def checkConstantExpr_impl[T](expr: c.Expr[T]): c.Tree = {
    expr.tree.foreach { t =>
      if (isAnnotatedWith(t.tpe.widen, inputAnnotType)) {
        c.abort(t.pos, s"Tree references expression input")
      }
    }
    expr.tree
  }

  def isNullable_impl[T: c.WeakTypeTag]: c.Tree =
    q"${typeOf[Null] <:< weakTypeOf[T]}"

  def tripleEquals_impl[A, B](right: c.Expr[B]): c.Tree = {
    val Apply(_, List(leftTree)) = c.prefix.tree
    val rightTree = right.tree

    val leftTpe = leftTree.tpe.widen
    val rightTpe = rightTree.tpe.widen

    lazy val leftToRightConv = c.inferImplicitView(leftTree, leftTree.tpe, rightTree.tpe.widen)
    lazy val rightToLeftConv = c.inferImplicitView(rightTree, rightTree.tpe, leftTree.tpe.widen)

    if (leftTpe <:< rightTpe)
      q"$leftTree == $rightTree"
    else if (rightTpe <:< leftTpe)
      q"$rightTree == $leftTree"
    else if (rightToLeftConv != EmptyTree)
      q"$leftTree == $rightToLeftConv($rightTree)"
    else if (leftToRightConv != EmptyTree)
      q"$leftToRightConv($leftTree) == $rightTree"
    else
      c.abort(c.enclosingPosition, s"Values of types $leftTpe and $rightTpe cannot be compared for equality")

  }

  def materializeTypeToken[T: c.WeakTypeTag]: c.Tree =
    q"new _root_.com.google.common.reflect.TypeToken[${weakTypeOf[T]}] {}"

  import scala.reflect.runtime.{universe => ru}

  def materializeTypeRepr[T: c.WeakTypeTag](tt: c.Expr[ru.TypeTag[T]]): c.Tree =
    q"$ScexPkg.TypeRepr(${showCode(TypeTree(weakTypeOf[T]))})"
}
