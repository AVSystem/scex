package com.avsystem.scex.validation

import SymbolValidator._
import com.avsystem.scex.util.CommonUtils._
import java.{util => ju, lang => jl}
import scala.language.dynamics
import scala.language.experimental.macros
import scala.language.implicitConversions

class SymbolValidator(val accessSpecs: List[MemberAccessSpec]) {

  val referencedJavaClasses = accessSpecs.collect({
    case MemberAccessSpec(typeInfo, _, _, true) if typeInfo.clazz.isDefined && typeInfo.isJava =>
      hierarchy(typeInfo.clazz.get)
  }).flatten.toSet

  private val lowestPriority = accessSpecs.length

  // MemberAccessSpec with its index in ACL (accessSpecs)
  private type SpecWithIndex = (MemberAccessSpec, Int)

  private val bySignaturesMap: Map[String, List[SpecWithIndex]] =
    accessSpecs.zipWithIndex.groupBy(_._1.memberSignature).toMap.withDefaultValue(Nil)

  def isMemberAccessAllowed(vc: ValidationContext)(access: vc.MemberAccess): vc.ValidationResult = {
    import vc.{c => _, _}

    def implicitConversionsMatch(actual: Option[ImplicitConversion], fromSpec: Option[(String, TypeInfo)]) =
      (actual, fromSpec) match {
        case (Some(ImplicitConversion(actualPathTree, actualImplicitTpe)), Some((specPath, specImplicitTypeInfo))) =>
          path(actualPathTree) == specPath && actualImplicitTpe <:< specImplicitTypeInfo.typeIn(vc.c.universe)
        case (None, None) => true
        case _ => false
      }

    access match {
      case access@SimpleMemberAccess(tpe, symbol, implicitConv, allowedByDefault, position) =>
        val signatures: List[String] =
          (symbol :: symbol.allOverriddenSymbols).map(memberSignature)

        // MemberAccessSpecs that match this invocation
        val matchingSpecs: List[SpecWithIndex] = signatures.flatMap { signature =>
          bySignaturesMap(signature).filter { case (accessSpec, _) =>
            signature == accessSpec.memberSignature &&
              tpe <:< accessSpec.typeInfo.typeIn(vc.c.universe) &&
              implicitConversionsMatch(implicitConv, accessSpec.implicitConv)
          }
        }

        // get 'allow' field from matching spec that appeared first in ACL or false if there was no matching spec
        val (allow, index) = if (matchingSpecs.nonEmpty) {
          val (spec, index) = matchingSpecs.minBy(_._2)
          (spec.allow, index)
        } else (allowedByDefault, lowestPriority)

        ValidationResult(index, if (allow) Nil else List(access))

      case MultipleMemberAccesses(accesses) =>
        // all must be allowed
        val (allowed, denied) = accesses.map(a => isMemberAccessAllowed(vc)(a)).partition(_.deniedAccesses.isEmpty)
        if (denied.nonEmpty)
          ValidationResult(denied.minBy(_.priority).priority, denied.flatMap(_.deniedAccesses))
        else if (allowed.nonEmpty)
          ValidationResult(allowed.maxBy(_.priority).priority, Nil)
        else
          ValidationResult(lowestPriority, Nil)

      case AlternativeMemberAccess(accesses) =>
        // take the soonest-validated alternative
        if (accesses.nonEmpty)
          accesses.map(a => isMemberAccessAllowed(vc)(a)).minBy(_.priority)
        else
          ValidationResult(lowestPriority, Nil)
    }
  }
}

object SymbolValidator {

  import SymbolValidatorMacros._

  private def elidedByMacro =
    throw new NotImplementedError("You cannot use this outside of symbol validator DSL")

  case class MemberAccessSpec(typeInfo: TypeInfo, memberSignature: String, implicitConv: Option[(String, TypeInfo)], allow: Boolean) {
    override def toString = {
      val implicitConvRepr = implicitConv match {
        case Some((implicitConvPath, implicitTypeInfo)) => s"when implicitly converted to |$implicitTypeInfo| by |$implicitConvPath|"
        case None => ""
      }
      (if (allow) "Allowed" else "Denied") + s" on type |$typeInfo| member |$memberSignature| $implicitConvRepr"
    }
  }

  implicit def toDirectWildcardSelector(any: Any): DirectWildcardSelector = elidedByMacro

  // indicates end of wildcard selector
  trait CompleteWildcardSelector extends Any

  trait WildcardSelector extends Any {
    /**
     * Starts "wildcard" notation to allow or deny calling multiple methods on some type,
     * in single DSL statement. Example:
     *
     * <pre>
     * on { s: String =>
     * &nbsp;&nbsp;s.all.declared.methods
     * }
     * </pre>
     *
     * @return
     */
    def all: ScopeSpecifiers with MemberSubsets
  }

  trait DirectWildcardSelector extends WildcardSelector {
    def all: ScopeSpecifiers with DirectMemberSubsets

    /**
     * Starts "wildcard" notation to allow or deny calling multiple methods available through implicit conversion
     * on some type, in single DSL statement. Example:
     *
     * <pre>
     * on { i: Int =>
     * &nbsp;&nbsp;i.implicitlyAs[RichInt].all.declared.methods
     * }
     * </pre>
     *
     * Implicit conversion used in SymbolValidator DSL must be globally visible (e.g. method in non-nested Scala object)
     *
     * @tparam T
     * @return
     */
    def implicitlyAs[T]: WildcardSelector

    def constructorWithSignature(signature: String): CompleteWildcardSelector
  }

  trait ScopeSpecifiers extends Any {
    /**
     * Filters out methods specified by "wildcard" notation leaving only these declared in class
     * that represents given type.
     * @return
     */
    def declared: ScalaMemberSubsets

    /**
     * Filters out methods specified by "wildcard" notation leaving only these introduced in class
     * that represents given type, where 'introduced' means declared and not overriding anything.
     * @return
     */
    def introduced: ScalaMemberSubsets
  }

  trait MemberSubsets extends Any {
    /**
     * Allows or denies calling all methods (not constructors) available for given type, on this type, except
     * for members from `Any`/`AnyVal`/`AnyRef`.
     *@return
     */
    def members: CompleteWildcardSelector

    /**
     * Allows or denies calling all overloaded variants of method with given name available for given type, on this type.
     * Uses Dynamic notation, for example:
     *
     * <pre>
     * on { s: String =>
     * &nbsp;&nbsp;s.all.membersNamed.getBytes
     * }
     * </pre>
     *
     * @return
     */
    def membersNamed: MethodsNamed

    /**
     * Allows or denies calling all methods with given names available for given type, on this type.
     * For example:
     *
     * <pre>
     * on { s: String =>
     * &nbsp;&nbsp;s.all.membersNamed("getBytes")
     * }
     * </pre>
     *
     * @return
     */
    def membersNamed(names: String*): CompleteWildcardSelector

    /**
     * Allows or denies calling all JavaBean getters available for given type, on this type.
     * JavaBean getter is defined as a method taking no parameters or type parameters,
     * with name starting with <tt>get</tt> (or <tt>is</tt> if it returns <tt>boolean</tt> or <tt>java.lang.Boolean</tt>)
     * followed by capitalized name of bean property.
     *
     * @return
     */
    def beanGetters: CompleteWildcardSelector

    /**
     * Allows or denies calling all JavaBean setters available for given type, on this type.
     * JavaBean setter is defined as a method returning <tt>void</tt>, taking single parameter and no type parameters,
     * with name starting with <tt>set</tt> followed by capitalized name of bean property.
     *
     * @return
     */
    def beanSetters: CompleteWildcardSelector
  }

  trait ScalaMemberSubsets extends MemberSubsets {
    /**
     * Allows or denies calling getters for Scala <tt>val</tt>s or <tt>var</tt>s available for given type, on this type.
     * @return
     */
    def scalaGetters: CompleteWildcardSelector

    /**
     * Allows or denies calling setters for Scala <tt>var</tt>s available for given type, on this type.
     * @return
     */
    def scalaSetters: CompleteWildcardSelector
  }

  trait DirectMemberSubsets extends ScalaMemberSubsets {
    /**
     * Allows or denies creating new instances of given type (or subtypes if they still correspond to the same class)
     * using any available constructor.
     * @return
     */
    def constructors: CompleteWildcardSelector
  }

  trait MethodsNamed extends Any with Dynamic {
    def selectDynamic(name: String): CompleteWildcardSelector = macro methodsNamed_selectDynamic_impl
  }

  /**
   * Encompases block of expressions that specify methods that are allowed to be called in expressions.
   * Multiple allow/deny blocks joined with <tt>++</tt> operator form an ACL-like structure.
   * @param expr
   * @return
   */
  def allow(expr: Any): List[SymbolValidator.MemberAccessSpec] = macro allow_impl

  /**
   * Encompases block of expressions that specify methods that are not allowed to be called in expressions.
   * Multiple allow/deny blocks joined with <tt>++</tt> operator form an ACL-like structure.
   * @param expr
   * @return
   */
  def deny(expr: Any): List[SymbolValidator.MemberAccessSpec] = macro deny_impl

  /**
   * Starts a block that allows or denies calling some methods on instances of some type. This is expressed using
   * lambda expression like this:
   *
   * <pre>
   * on { s: String =>
   * &nbsp;&nbsp;s.compareTo _
   * &nbsp;&nbsp;s.charAt _
   * &nbsp;&nbsp;s.length
   * }
   * @param expr
   * @tparam T
   * @return
   */
  def on[T](expr: T => Any): T => Any = macro on_impl[T]

  /**
   * Starts "wildcard" notation to allow or deny calling multiple Java static methods, with single DSL statement.
   * For example:
   *
   * <pre>
   * allStatic[String].membersNamed.valueOf
   * </pre>
   *
   * @tparam T
   * @return
   */
  def allStatic[T]: MemberSubsets = elidedByMacro
}
