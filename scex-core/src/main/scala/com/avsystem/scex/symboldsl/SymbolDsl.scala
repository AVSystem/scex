package com.avsystem.scex.symboldsl

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.language.{dynamics, implicitConversions}

/**
 * Author: ghik
 * Created: 11/14/14.
 */
trait SymbolDsl {
  type Payload

  private def stub =
    throw new NotImplementedError("You cannot use this outside of symbol DSL")

  @compileTimeOnly("You cannot use this outside of symbol DSL")
  implicit def toDirectWildcardSelector(any: Any): DirectWildcardSelector = stub

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
   * @tparam A
   * @return
   */
  def on[A](expr: A => Any): A => Any = macro SymbolInfoParser.on_impl[A]

  /**
   * Starts "wildcard" notation to allow or deny calling multiple Java static methods, with single DSL statement.
   * For example:
   *
   * <pre>
   * allStatic[String].membersNamed.valueOf
   * </pre>
   *
   * @tparam A
   * @return
   */
  @compileTimeOnly("You cannot use this outside of symbol DSL")
  def allStatic[A]: MemberSubsets = stub

  trait AttachedPayload

  trait AttachPayload {
    /**
     * Attaches some payload to specified member or members on given type.
     */
    @compileTimeOnly("You cannot use this outside of symbol DSL")
    def -->(payload: Payload): AttachedPayload
  }

  @compileTimeOnly("You cannot use this outside of symbol DSL")
  implicit def payloadAttach(any: Any): AttachPayload = stub

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

    def as[S]: S

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
     * Implicit conversion used in Symbol DSL must be globally visible (e.g. method in non-nested Scala object)
     *
     * @tparam A
     * @return
     */
    def implicitlyAs[A]: WildcardSelector

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
     * @return
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
    def membersNamed: MembersNamed

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

  trait MembersNamed extends Any with Dynamic {
    def selectDynamic(name: String): CompleteWildcardSelector = stub
  }

  /**
   * Type annotation that serves as an alternative way of expressing existential types (types with wildcards) with
   * higher bounds in the symbol DSL.
   * </p>
   * For example, imagine you want to allow invoking <tt>insert</tt> method on all lists of type
   * <tt>java.util.List[_ <: Number]</tt>. Unfortunately, the following will NOT work:
   * <pre>
   * allow {
   * on { l: java.util.List[_ <: Number] =>
   * l.insert _  // won't typecheck
   * }
   * }
   * </pre>
   * </p>
   * Scala typechecker will report an error on <tt>l.insert _</tt> because this method cannot be called
   * when the element type of the list is unknown.
   * </p>
   * To overcome this limitation, you can use alternative syntax to express the same:
   * <pre>
   * allow {
   * on { l: java.util.List[Number@plus] =>
   * l.insert _
   * }
   * }
   * </pre>
   */
  class plus extends StaticAnnotation

  /**
   * Analogous to {@link com.avsystem.scex.validation.SymbolDsl.plus plus},
   * but used to express wildcards with lower bounds, e.g. <tt>java.lang.List[_ >: String]</tt>
   */
  class minus extends StaticAnnotation

}
