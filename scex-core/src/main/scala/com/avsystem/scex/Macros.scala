package com.avsystem.scex

import com.avsystem.scex.compiler.annotation.BooleanIsGetter
import com.avsystem.scex.util.{Literal => ScexLiteral, MacroUtils}
import java.{util => ju, lang => jl}
import scala.annotation.tailrec
import scala.reflect.macros.Context
import scala.util.control.NonFatal

/**
 * Created: 18-11-2013
 * Author: ghik
 */
object Macros {
  def javaGetter_impl[T: c.WeakTypeTag](c: Context): c.Expr[T] = {
    val macroUtils = MacroUtils(c.universe)

    import c.universe._
    import macroUtils._

    val booleanIsGetter = c.macroApplication.symbol.annotations.exists(_.tpe <:< typeOf[BooleanIsGetter])
    def javaGetter(propertyName: String) =
      (if (booleanIsGetter) "is" else "get") + propertyName.capitalize

    c.macroApplication match {
      case Select(ImplicitlyConverted(prefix, _), TermName(propertyName)) =>
        c.Expr[T](Select(prefix, newTermName(javaGetter(propertyName))))

      case Select(prefix@Ident(_), TermName(propertyName))
        if prefix.symbol.annotations.exists(_.tpe <:< rootAdapterAnnotType) =>

        c.Expr[T](Select(Select(prefix, newTermName("wrapped")), newTermName(javaGetter(propertyName))))
    }

  }

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
    val plusName = newTermName("+").encodedName

    def reifyConcatenation(parts: List[Tree], args: List[Tree]) = {

      @tailrec
      def reifyConcatenationIn(parts: List[Tree], args: List[Tree], result: Tree): Tree =
        (parts, args) match {

          case (part :: partsTail, arg :: argsTail) =>
            val withArg = Apply(Select(result, plusName), List(arg))
            val withPart = Apply(Select(withArg, plusName), List(part))
            reifyConcatenationIn(partsTail, argsTail, withPart)

          case (Nil, Nil) =>
            result

          case _ =>
            throw new IllegalArgumentException
        }

      val firstPart :: partsRest = parts
      reifyConcatenationIn(partsRest, args, firstPart)
    }

    def isBlankStringLiteral(tree: Tree) = tree match {
      case Literal(Constant(str: String)) if str.trim.isEmpty => true
      case _ => false
    }

    if (typeOf[String] <:< resultType) {
      c.Expr[T](reifyConcatenation(parts, argTrees))
    }
    // special cases for Java enums as there is no way to create general implicit conversion to arbitrary java enum
    // due to https://issues.scala-lang.org/browse/SI-7609
    else if (resultType <:< typeOf[jl.Enum[_]] && args.size == 0) {
      val enumModuleSymbol = resultType.typeSymbol.companionSymbol
      val Literal(Constant(stringLiteral: String)) = parts.head

      c.Expr[T](Select(Ident(enumModuleSymbol), newTermName(stringLiteral.trim)))

    } else if (resultType <:< typeOf[jl.Enum[_]] && args.size == 1 && parts.forall(isBlankStringLiteral) && !(args.head.actualType <:< resultType)) {
      val enumModuleSymbol = resultType.typeSymbol.companionSymbol

      c.Expr[T](Apply(Select(Ident(enumModuleSymbol), newTermName("valueOf")), List(args.head.tree)))

    } else if (args.size == 1 && parts.forall(isBlankStringLiteral)) {
      c.Expr[T](args.head.tree)

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
      c.error(c.enclosingPosition, s"Values of types $leftTpe and $rightTpe cannot be compared")
      null
    }
  }

  def qmark_impl(c: Context)(rhs: c.Expr[Any]): c.Expr[Any] = {
    import c.universe._

    val Apply(_, List(lhsTree)) = c.prefix.tree
    val lhs = c.Expr[Any](lhsTree)

    reify {
      def fallback = rhs.splice
      try {
        val result = lhs.splice
        if (result != null) result else fallback
      } catch {
        case _: NullPointerException => fallback
      }
    }
  }

  def literalTo[T: c.WeakTypeTag](c: Context)(lit: c.Expr[ScexLiteral], compileConversion: ScexLiteral => T, runtimeConversion: c.Expr[ScexLiteral => T]): c.Expr[T] = {
    import c.universe._

    val literalCompanionApplySymbol = typeOf[ScexLiteral.type].member(newTermName("apply"))

    lit.tree match {
      case Apply(literalCompanionApply, List(stringLiteralTree@Literal(Constant(literalString: String))))
        if literalCompanionApply.symbol == literalCompanionApplySymbol =>

        try {
          c.Expr[T](Literal(Constant(compileConversion(ScexLiteral(literalString)))))
        } catch {
          case NonFatal(_) =>
            c.error(stringLiteralTree.pos, s"""Cannot parse "$literalString" as a literal value of type ${weakTypeOf[T]}""")
            null
        }

      case _ =>
        reify(runtimeConversion.splice(lit.splice))
    }
  }

}
