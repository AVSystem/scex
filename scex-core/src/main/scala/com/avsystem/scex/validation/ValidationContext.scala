package com.avsystem.scex.validation

import com.avsystem.scex.compiler.annotation._
import com.avsystem.scex.util.MacroUtils
import scala.reflect.macros.Universe
import scala.runtime.StringAdd

abstract class ValidationContext protected extends MacroUtils {

  import universe._

  val contextTpe: Type

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
  lazy val wrappedInAdapterAnnotType = typeOf[WrappedInAdapter]

  lazy val any2stringadd = typeOf[Predef.type].member(newTermName("any2stringadd"))
  lazy val stringAddPlus = typeOf[StringAdd].member(newTermName("+").encodedName)
  lazy val stringConcat = typeOf[String].member(newTermName("+").encodedName)
  lazy val stringTpe = typeOf[String]

  def isExpressionUtil(symbol: Symbol): Boolean =
    symbol != null && symbol != NoSymbol &&
      (isExpressionUtilObject(symbol) || isExpressionUtil(symbol.owner))

  def isExpressionUtilObject(symbol: Symbol): Boolean =
    symbol != null && symbol != NoSymbol && symbol.annotations.exists(_.tpe =:= expressionUtilAnnotType)

  def isProfileObject(symbol: Symbol) =
    symbol != null && symbol.annotations.exists(_.tpe =:= profileObjectAnnotType)

  def isScexSynthetic(symbol: Symbol): Boolean =
    symbol != null && symbol != NoSymbol &&
      (isProfileObject(symbol) || isScexSynthetic(symbol.owner))

  def isAdapter(symbol: Symbol): Boolean =
    symbol != NoSymbol && symbol.annotations.exists(_.tpe =:= adapterAnnotType)

  def isAdapter(tpe: Type): Boolean =
    tpe != NoType && isAdapter(tpe.typeSymbol)

  def isWrappedInAdapter(symbol: Symbol): Boolean =
    symbol != NoSymbol && symbol.annotations.exists(_.tpe =:= wrappedInAdapterAnnotType)

  def isContextAdapter(symbol: Symbol) =
    symbol.annotations.exists(_.tpe =:= contextAdapterAnnotType)

  def isBooleanGetterAdapter(symbol: Symbol) =
    symbol.annotations.exists(_.tpe =:= booleanGetterAnnotType)

  // gets Java getter called by implicit wrapper
  def getJavaGetter(symbol: Symbol, javaTpe: Type): Symbol = {
    val prefix = if (isBooleanGetterAdapter(symbol)) "is" else "get"
    val name = prefix + symbol.name.toString.capitalize

    def fail = throw new Error(s"Could not find Java getter $name on $javaTpe")
    alternatives(javaTpe.member(newTermName(name))).find(isBeanGetter).getOrElse(fail)
  }

  def toStringAccess(tree: Tree) =
    SimpleMemberAccess(tree.tpe, toStringSymbol(tree.tpe), None, allowedByDefault = false, tree.pos)

  def needsValidation(symbol: Symbol) =
    symbol != null && symbol.isTerm && !symbol.isPackage && !isExpressionUtil(symbol) && !isProfileObject(symbol)

  def extractAccess(tree: Tree, staticAccessAllowedByDefault: Boolean = false): MemberAccess = {
    tree match {
      case Select(contextAdapter@Ident(_), _) if isContextAdapter(contextAdapter.symbol) && !isWrappedInAdapter(tree.symbol) =>
        val symbol = getJavaGetter(tree.symbol, contextTpe)

        SimpleMemberAccess(contextTpe, symbol, None, allowedByDefault = false, tree.pos)

      case Select(apply@ImplicitlyConverted(qualifier, fun), _) if !isScexSynthetic(fun.symbol) =>
        val accessByImplicit = SimpleMemberAccess(qualifier.tpe, tree.symbol,
          Some(ImplicitConversion(stripTypeApply(fun), apply.tpe)), allowedByDefault = false, tree.pos)

        val implicitConversionAccess = extractAccess(fun)
        val plainAccess = SimpleMemberAccess(apply.tpe, tree.symbol, None, allowedByDefault = false, tree.pos)
        var alternatives = List(accessByImplicit, MultipleMemberAccesses(List(implicitConversionAccess, plainAccess)))

        // special case for configuration convenience: 'any + <string>' (using any2stringadd) is also validated as
        // combination of toString and string concatenation
        if (fun.symbol == any2stringadd && tree.symbol == stringAddPlus) {
          val toStringAccess = SimpleMemberAccess(qualifier.tpe, toStringSymbol(qualifier.tpe), None, allowedByDefault = false, tree.pos)
          val stringConcatAccess = SimpleMemberAccess(stringTpe, stringConcat, None, allowedByDefault = false, tree.pos)
          alternatives = MultipleMemberAccesses(List(toStringAccess, stringConcatAccess)) :: alternatives
        }

        MultipleMemberAccesses(List(AlternativeMemberAccess(alternatives), extractAccess(qualifier)))

      case Select(apply@ImplicitlyConverted(qualifier, fun), _) if isAdapter(apply.tpe) =>
        val symbol = getJavaGetter(tree.symbol, qualifier.tpe)
        val access = SimpleMemberAccess(qualifier.tpe, symbol, None, allowedByDefault = false, tree.pos)

        MultipleMemberAccesses(List(access, extractAccess(qualifier)))

      case Select(qualifier, _) if needsValidation(tree.symbol) =>
        val access = SimpleMemberAccess(qualifier.tpe, tree.symbol, None, staticAccessAllowedByDefault, tree.pos)
        // When accessing member of static module (that includes Java statics) not from Any/AnyVal/AnyRef
        // the static module access itself is allowed by default.
        val staticMember = isStaticModule(qualifier.symbol) && !isFromToplevelType(tree.symbol)
        MultipleMemberAccesses(List(access, extractAccess(qualifier, staticMember)))

      // special case for configuration convenience: string concatenation also forces validation of toString on its argument
      case Apply(qualifier, List(arg)) if qualifier.symbol == stringConcat =>
        MultipleMemberAccesses(List(toStringAccess(arg), extractAccess(qualifier), extractAccess(arg)))

      // special case for configuration convenience: standard string interpolations also force validation of
      // toString on its arguments
      case Apply(qualifier, args) if standardStringInterpolations contains qualifier.symbol =>
        val toStringAccesses = MultipleMemberAccesses(args.map(toStringAccess))
        MultipleMemberAccesses(toStringAccesses :: extractAccess(qualifier) :: args.map(arg => extractAccess(arg)))

      case _ =>
        MultipleMemberAccesses(tree.children.map(child => extractAccess(child)))
    }
  }
}

object ValidationContext {
  def apply(u: Universe)(contextType: u.Type) = new ValidationContext {
    val universe: u.type = u
    val contextTpe = contextType
  }
}