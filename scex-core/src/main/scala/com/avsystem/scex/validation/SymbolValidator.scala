package com.avsystem.scex.validation

import SymbolValidator._
import com.avsystem.scex.util.CommonUtils._
import com.avsystem.scex.util.MacroUtils
import java.{util => ju, lang => jl}
import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.language.dynamics
import scala.reflect.macros.Context

class SymbolValidator(val accessSpecs: List[MemberAccessSpec]) {

  lazy val referencedJavaClasses = accessSpecs.collect({
    case MemberAccessSpec(typeInfo, _, _, _, true)
      if typeInfo.clazz.isDefined && typeInfo.isJava =>
      hierarchy(typeInfo.clazz.get)
  }).flatten.toSet

  private type SignatureWithConversion = (String, String)
  // MemberAccessSpec with its index in ACL (accessSpecs)
  private type SpecWithIndex = (MemberAccessSpec, Int)

  private val bySignaturesMap: Map[SignatureWithConversion, List[SpecWithIndex]] =
    accessSpecs.zipWithIndex.groupBy {
      case (MemberAccessSpec(_, methodSignature, implicitConvSignature, _, _), _) =>
        (methodSignature, implicitConvSignature)
    }.toMap.withDefaultValue(Nil)

  def isInvocationAllowed(c: Context)(
    objType: c.universe.Type,
    invokedSymbol: c.universe.Symbol,
    implicitConv: c.universe.Tree,
    implicitTpe: c.universe.Type): Boolean = {

    val macroUtils = MacroUtils(c)
    import macroUtils.{c => _, _}

    val implicitConvPath = path(implicitConv)

    // signatures of invoked symbol and all symbols overridden by it
    val allSignatures: List[SignatureWithConversion] =
      (invokedSymbol :: invokedSymbol.allOverriddenSymbols).map {
        symbol => (memberSignature(symbol), implicitConvPath)
      }.toSet.toList

    // MemberAccessSpecs that match this invocation
    val matchingSpecs: List[SpecWithIndex] = allSignatures.flatMap { signature =>
      bySignaturesMap(signature).filter {
        case (accessSpec, _) =>
          objType <:< accessSpec.typeInfo.typeIn(c.universe) &&
            implicitTpe <:< accessSpec.implicitTypeInfo.typeIn(c.universe) // NoType <:< NoType returns true
      }
    }

    // get 'allow' field from matching spec that appeared first in ACL or false if there was no matching spec
    if (matchingSpecs.nonEmpty) matchingSpecs.minBy(_._2)._1.allow else false
  }

}

object SymbolValidator {

  import SymbolValidatorMacros._

  private def elidedByMacro =
    throw new NotImplementedError("You cannot use this outside of symbol validator DSL")

  case class MemberAccessSpec(typeInfo: TypeInfo, member: String, implicitConvPath: String, implicitTypeInfo: TypeInfo, allow: Boolean) {
    override def toString = {
      (if (allow) "Allowed" else "Denied") +
        s" on type $typeInfo method $member" +
        s" implicitly converted to $implicitTypeInfo by $implicitConvPath"
    }
  }

  implicit def toDirectWildcardSelector(any: Any): DirectWildcardSelector = elidedByMacro

  // indicates end of wildcard selector
  trait CompleteWildcardSelector

  trait WildcardSelector {
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
    def all: ScopeSpecifiers with MethodSubsets
  }

  trait DirectWildcardSelector extends WildcardSelector {
    def all: ScopeSpecifiers with DirectMethodSubsets

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
     * @tparam T
     * @return
     */
    def implicitlyAs[T]: WildcardSelector
  }

  trait ScopeSpecifiers {
    /**
     * Filters out methods specified by "wildcard" notation leaving only these declared in class
     * that represents given type.
     * @return
     */
    def declared: ScalaMethodSubsets

    /**
     * Filters out methods specified by "wildcard" notation leaving only these introduced in class
     * that represents given type, where 'introduced' means declared and not overriding anything.
     * @return
     */
    def introduced: ScalaMethodSubsets
  }

  trait MethodSubsets {
    /**
     * Allows or denies calling all methods (not constructors) available for given type, on this type.
     * @return
     */
    def methods: CompleteWildcardSelector

    /**
     * Allows or denies calling all overloaded variants of method with given name available for given type, on this type.
     * Uses Dynamic notation, for example:
     *
     * <pre>
     * on { s: String =>
     * &nbsp;&nbsp;s.all.methodsNamed.getBytes
     * }
     * </pre>
     *
     * @return
     */
    def methodsNamed: MethodsNamed

    /**
     * Allows or denies calling all overloaded variants of method with given name available for given type, on this type.
     * For example:
     *
     * <pre>
     * on { s: String =>
     * &nbsp;&nbsp;s.all.methodsNamed("getBytes")
     * }
     * </pre>
     *
     * @return
     */
    def methodsNamed(name: String): CompleteWildcardSelector = macro methodsNamed_impl

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

  trait ScalaMethodSubsets extends MethodSubsets {
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

  trait DirectMethodSubsets extends ScalaMethodSubsets {
    /**
     * Allows or denies creating new instances of given type (or subtypes if they still correspond to the same class)
     * using any available constructor.
     * @return
     */
    def constructors: CompleteWildcardSelector
  }

  trait MethodsNamed extends Dynamic {
    def selectDynamic(name: String): CompleteWildcardSelector
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
   * allStatic[String].methodsNamed.valueOf
   * </pre>
   *
   * @tparam T
   * @return
   */
  def allStatic[T]: MethodSubsets = elidedByMacro
}
