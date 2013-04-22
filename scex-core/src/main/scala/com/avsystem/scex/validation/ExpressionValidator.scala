package com.avsystem.scex.validation

import java.{util => ju, lang => jl}
import scala.reflect.macros.Context
import scala.language.experimental.macros
import com.avsystem.scex.{BooleanIsGetter, JavaGettersAdapter, ExpressionProfile}
import scala.util.DynamicVariable
import com.avsystem.scex.Utils._

/**
 * Object used during expression compilation to validate the expression (syntax, invocations, etc.)
 * This must be a Scala object and not a class because it contains macros. Validation is performed against
 * given ExpressionProfile which is injected into this object by ExpressionCompiler by means of a dynamic variable.
 */
object ExpressionValidator {
  val profile: DynamicVariable[ExpressionProfile] = new DynamicVariable(null)

  def validate[T](expr: T): T = macro validate_impl[T]

  def validate_impl[T](c: Context)(expr: c.Expr[T]): c.Expr[T] = {
    import c.universe._

    expr.tree.foreach {
      subtree =>
        if (!profile.value.syntaxValidator.isSyntaxAllowed(c.universe)(subtree)) {
          c.error(subtree.pos, s"Cannot use language construct: ${subtree.getClass.getSimpleName}")
        }
    }

    def needsValidation(symbol: Symbol) =
      symbol != null && (symbol.isMethod || isJavaField(symbol))

    def validateAccess(pos: Position, tpe: Type, symbol: Symbol, icSymbol: Option[Symbol]) {
      if (needsValidation(symbol)) {
        if (!profile.value.accessValidator.isInvocationAllowed(c)(tpe, symbol, icSymbol)) {
          c.error(pos, s"Cannot call ${memberSignature(symbol)} on ${tpe.typeSymbol.fullName}")
        }
      }
    }

    lazy val adapterType = typeOf[JavaGettersAdapter]
    lazy val booleanIsGetterType = typeOf[BooleanIsGetter]

    // gets Java getter called by implicit wrapper
    def getJavaGetter(symbol: Symbol, javaTpe: Type): Symbol = {
      val prefix =
        if (symbol.annotations.exists(_.tpe =:= booleanIsGetterType))
          "is"
        else
          "get"

      val name = prefix + symbol.name.toString.capitalize

      def fail = throw new RuntimeException(s"Could not get java getter for $symbol on $javaTpe")

      javaTpe.member(newTermName(name)) match {
        case s if isJavaParameterlessMethod(s) => s
        case overloaded: TermSymbol => overloaded.alternatives.find(isJavaParameterlessMethod) match {
          case Some(s) => s
          case None => fail
        }
        case _ => fail
      }
    }

    def validateTree(tree: Tree) {
      tree match {
        case tree@Select(apply@Apply(fun, List(qualifier)), _) if isStaticImplicitConversion(fun.symbol) && apply.pos == qualifier.pos =>
          val wrappedJavaGetter = apply.tpe != null && apply.tpe <:< adapterType

          if (wrappedJavaGetter) {
            validateAccess(tree.pos, qualifier.tpe, getJavaGetter(tree.symbol, qualifier.tpe), None)
          } else {
            validateAccess(tree.pos, qualifier.tpe, tree.symbol, Some(fun.symbol))
          }

          validateTree(qualifier)

        case tree@Select(qualifier, _) =>
          validateAccess(tree.pos, qualifier.tpe, tree.symbol, None)
          validateTree(qualifier)

        case _ =>
          tree.children.foreach(child => validateTree(child))
      }
    }

    validateTree(expr.tree)

    expr
  }
}
