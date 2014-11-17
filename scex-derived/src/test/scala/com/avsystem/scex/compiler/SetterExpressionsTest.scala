package com.avsystem.scex
package compiler

import java.{lang => jl, util => ju}

import com.avsystem.scex.util.SimpleContext

import scala.reflect.runtime.universe.TypeTag

class SetterTarget {
  var costam = 0

  private var _lol = 0

  def lol = _lol

  def lol_=(lol: Int) = {
    _lol = lol
  }

}

/**
 * Created: 28-11-2013
 * Author: ghik
 */
class SetterExpressionsTest extends ScexFunSuite with CompilationTest {

  import com.avsystem.scex.validation.SymbolValidator._

  def applySetter[R: TypeTag, T: TypeTag](expr: String, root: R, value: T,
                                          acl: List[MemberAccessSpec] = PredefinedAccessSpecs.basicOperations) = {

    val setterExpression = compiler.getCompiledSetterExpression[SimpleContext[R], T](createProfile(acl), expr, template = false)
    setterExpression.apply(SimpleContext(root)).apply(value)
  }

  test("scala variable setter test") {
    val target = new SetterTarget
    applySetter("costam", target, 42, allow(on { st: SetterTarget => st.all.introduced.members}))
    assert(42 === target.costam)
  }

  test("scala setter test") {
    val target = new SetterTarget
    applySetter("lol", target, 42, allow(on { st: SetterTarget => st.all.introduced.members}))
    assert(42 === target.lol)
  }

  test("direct bean setter test") {
    val target = new JavaSetterTarget
    applySetter("getBeanprop", target, 42, allow(on { st: JavaSetterTarget => st.all.introduced.members}))
    assert(42 === target.getBeanprop)
  }

  test("adapted root bean setter test") {
    val target = new JavaSetterTarget
    applySetter("beanprop", target, 42, allow(on { st: JavaSetterTarget => st.all.introduced.members}))
    assert(42 === target.getBeanprop)
  }

  test("adapted non-root bean setter test") {
    val target = new JavaSetterTarget
    applySetter("self.beanprop", target, 42, allow(on { st: JavaSetterTarget => st.all.introduced.members}))
    assert(42 === target.self.getBeanprop)
  }

  test("java field test") {
    val target = new JavaSetterTarget
    applySetter("field", target, 42, allow(on { st: JavaSetterTarget => st.all.introduced.members}))
    assert(42 === target.field)
  }

  test("dynamic setter test") {
    val setterExpression = compiler.getCompiledSetterExpression[SimpleContext[Unit], String](
      createProfile(Nil), "_vars.lol", template = false)

    val context = SimpleContext(())
    setterExpression.apply(context).apply("42")
    assert("42" === context.getVariable("lol"))
  }

}
