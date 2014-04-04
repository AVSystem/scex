package com.avsystem.scex

import com.avsystem.scex.util.{Literal => ScexLiteral, MacroUtils}
import java.{util => ju, lang => jl}
import scala.reflect.macros.Context
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
        c.inferImplicitValue(TypeRef(templateInterpolationsObjectTpe, splicerSymbol, List(arg.tpe))) match {
          case EmptyTree => Select(arg, newTermName("toString")).setPos(arg.pos)
          case tree => Apply(Select(tree, newTermName("toString")).setPos(arg.pos), List(arg)).setPos(arg.pos)
        }
      }

      Apply(Apply(Select(templateInterpolationsObjectTree, newTermName("concat")), parts), convertedArgs)
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
      val enumModuleSymbol = resultType.typeSymbol.companionSymbol
      val Literal(Constant(stringLiteral: String)) = parts.head

      c.Expr[T](Select(Ident(enumModuleSymbol), newTermName(stringLiteral)))

    } else if (resultType <:< typeOf[jl.Enum[_]] && args.size == 1 && parts.forall(isEmptyStringLiteral) && !(args.head.actualType <:< resultType)) {
      val enumModuleSymbol = resultType.typeSymbol.companionSymbol

      c.Expr[T](Apply(Select(Ident(enumModuleSymbol), newTermName("valueOf")), List(args.head.tree)))

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

  def literalToBoolean_impl(c: Context)(lit: c.Expr[ScexLiteral]): c.Expr[Boolean] =
    literalTo(c)(lit, _.toBoolean, c.universe.reify((lit: ScexLiteral) => lit.toBoolean))

  def literalToJBoolean_impl(c: Context)(lit: c.Expr[ScexLiteral]): c.Expr[jl.Boolean] =
    literalTo(c)(lit, _.toBoolean, c.universe.reify((lit: ScexLiteral) => lit.toBoolean))

  def literalToChar_impl(c: Context)(lit: c.Expr[ScexLiteral]): c.Expr[Char] =
    literalTo(c)(lit, _.toChar, c.universe.reify((lit: ScexLiteral) => lit.toChar))

  def literalToJCharacter_impl(c: Context)(lit: c.Expr[ScexLiteral]): c.Expr[jl.Character] =
    literalTo(c)(lit, _.toChar, c.universe.reify((lit: ScexLiteral) => lit.toChar))

  def literalToByte_impl(c: Context)(lit: c.Expr[ScexLiteral]): c.Expr[Byte] =
    literalTo(c)(lit, _.toByte, c.universe.reify((lit: ScexLiteral) => lit.toByte))

  def literalToJByte_impl(c: Context)(lit: c.Expr[ScexLiteral]): c.Expr[jl.Byte] =
    literalTo(c)(lit, _.toByte, c.universe.reify((lit: ScexLiteral) => lit.toByte))

  def literalToShort_impl(c: Context)(lit: c.Expr[ScexLiteral]): c.Expr[Short] =
    literalTo(c)(lit, _.toShort, c.universe.reify((lit: ScexLiteral) => lit.toShort))

  def literalToJShort_impl(c: Context)(lit: c.Expr[ScexLiteral]): c.Expr[jl.Short] =
    literalTo(c)(lit, _.toShort, c.universe.reify((lit: ScexLiteral) => lit.toShort))

  def literalToInt_impl(c: Context)(lit: c.Expr[ScexLiteral]): c.Expr[Int] =
    literalTo(c)(lit, _.toInt, c.universe.reify((lit: ScexLiteral) => lit.toInt))

  def literalToJInteger_impl(c: Context)(lit: c.Expr[ScexLiteral]): c.Expr[jl.Integer] =
    literalTo(c)(lit, _.toInt, c.universe.reify((lit: ScexLiteral) => lit.toInt))

  def literalToLong_impl(c: Context)(lit: c.Expr[ScexLiteral]): c.Expr[Long] =
    literalTo(c)(lit, _.toLong, c.universe.reify((lit: ScexLiteral) => lit.toLong))

  def literalToJLong_impl(c: Context)(lit: c.Expr[ScexLiteral]): c.Expr[jl.Long] =
    literalTo(c)(lit, _.toLong, c.universe.reify((lit: ScexLiteral) => lit.toLong))

  def literalToFloat_impl(c: Context)(lit: c.Expr[ScexLiteral]): c.Expr[Float] =
    literalTo(c)(lit, _.toFloat, c.universe.reify((lit: ScexLiteral) => lit.toFloat))

  def literalToJFloat_impl(c: Context)(lit: c.Expr[ScexLiteral]): c.Expr[jl.Float] =
    literalTo(c)(lit, _.toFloat, c.universe.reify((lit: ScexLiteral) => lit.toFloat))

  def literalToDouble_impl(c: Context)(lit: c.Expr[ScexLiteral]): c.Expr[Double] =
    literalTo(c)(lit, _.toDouble, c.universe.reify((lit: ScexLiteral) => lit.toDouble))

  def literalToJDouble_impl(c: Context)(lit: c.Expr[ScexLiteral]): c.Expr[jl.Double] =
    literalTo(c)(lit, _.toDouble, c.universe.reify((lit: ScexLiteral) => lit.toDouble))
}
