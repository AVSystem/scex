package com.avsystem.scex.validation

import scala.reflect.macros.Context
import com.avsystem.scex.util.MacroUtils
import com.avsystem.scex.compiler.annotation._

abstract class ValidationContext protected extends MacroUtils {
  val c: Context

  import c.universe._

  sealed abstract class MemberAccess {
    def repr: String = repr("")

    private def repr(prefix: String): String = this match {
      case SimpleMemberAccess(tpe, symbol, implicitConv, allowedByDefault, _) =>
        val implicitConvRepr = implicitConv match {
          case Some(ImplicitConversion(tree, implicitTpe)) =>
            s" implicitly converted by |${path(tree)}| to |$implicitTpe|"
          case None => ""
        }

        s"$prefix|${memberSignature(symbol)}| on type |$tpe|$implicitConvRepr " +
          (if (allowedByDefault) "allowed" else "denied") + " by default"

      case MultipleMemberAccesses(accesses) =>
        val reprs = accesses.map(_.repr(prefix)).filter(_.nonEmpty)
        reprs.mkString("\n")

      case AlternativeMemberAccess(accesses) =>
        val reprs = accesses.map(_.repr(prefix + " ")).filter(_.nonEmpty)
        reprs.mkString(prefix + "--\n", "\n" + prefix + "OR\n", "\n" + prefix + "--")
    }
  }

  // Denotes occurence of implicit conversion
  case class ImplicitConversion(tree: Tree, tpe: Type) {
    override def toString =
      s"implicit conversion ${path(tree)} to $tpe"
  }

  case class SimpleMemberAccess(tpe: Type, symbol: Symbol, implicitConv: Option[ImplicitConversion],
    allowedByDefault: Boolean, pos: Position) extends MemberAccess {

    override def toString =
      s"${memberSignature(symbol)} on type $tpe" + implicitConv.map(ic => s" by $ic").getOrElse("")
  }

  // Multiple accesses, all must be allowed
  case class MultipleMemberAccesses(accesses: List[MemberAccess]) extends MemberAccess

  // An access that can be expressed in multiple ways - at least one of them must be allowed
  // before any other is deniedAccesses ("before" means earlier position in ACL)
  case class AlternativeMemberAccess(accesses: List[MemberAccess]) extends MemberAccess

  case class ValidationResult(priority: Int, deniedAccesses: List[SimpleMemberAccess])

  lazy val adapterAnnotType = typeOf[JavaGetterAdapter]
  lazy val booleanGetterAnnotType = typeOf[BooleanIsGetter]
  lazy val contextAdapterAnnotType = typeOf[ContextAdapter]
  lazy val expressionUtilAnnotType = typeOf[ExpressionUtil]
  lazy val profileObjectAnnotType = typeOf[ProfileObject]

  def isExpressionUtil(symbol: Symbol): Boolean =
    symbol != null && symbol != NoSymbol &&
      (symbol.annotations.exists(_.tpe =:= expressionUtilAnnotType) || isExpressionUtil(symbol.owner))

  def isProfileObject(symbol: Symbol) =
    symbol != null && symbol.annotations.exists(_.tpe =:= profileObjectAnnotType)

  def isScexSynthetic(symbol: Symbol): Boolean =
    symbol != null && symbol != NoSymbol &&
      (isProfileObject(symbol) || isScexSynthetic(symbol.owner))

  def isAdapter(tpe: Type) =
    tpe != NoType && tpe.typeSymbol.annotations.exists(_.tpe =:= adapterAnnotType)

  def isContextAdapter(symbol: Symbol) =
    symbol.annotations.exists(_.tpe =:= contextAdapterAnnotType)

  def isBooleanGetterAdapter(symbol: Symbol) =
    symbol.annotations.exists(_.tpe =:= booleanGetterAnnotType)

  // gets Java getter called by implicit wrapper
  def getJavaGetter(symbol: Symbol, javaTpe: Type): Symbol = {
    val prefix = if (isBooleanGetterAdapter(symbol)) "is" else "get"
    val name = prefix + symbol.name.toString.capitalize

    def fail = throw new Error(s"Could not find Java getter for $name on $javaTpe")
    javaTpe.member(newTermName(name)).asTerm.alternatives.find(isBeanGetter).getOrElse(fail)
  }
}

object ValidationContext {
  def apply(ctx: Context) = new ValidationContext {
    val c: ctx.type = ctx
  }
}