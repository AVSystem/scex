package com.avsystem.scex
package compiler

import com.avsystem.commons.jiop.JavaInterop._
import com.avsystem.scex.compiler.TemplateInterpolations.Splicer
import com.avsystem.scex.util.{PredefinedAccessSpecs, SimpleContext}
import com.github.ghik.silencer.silent
import org.scalatest.FunSuite

import scala.reflect.runtime.universe.TypeTag

class SetterTarget {
  var costam = 0

  private var _lol = 0

  def lol = _lol

  def lol_=(lol: Int) = {
    _lol = lol
  }

}

object SetterConversions {
  implicit val stringJBooleanConv: SetterConversion[String, JBoolean] =
    SetterConversion(_.toBoolean)
}

object CustomBooleanSplicer {
  implicit val jBooleanSplicer: Splicer[JBoolean] =
    new Splicer[JBoolean] {
      def toString(b: JBoolean) = b.toString
    }
}

/**
  * Created: 28-11-2013
  * Author: ghik
  */
@silent("a pure expression does nothing in statement position")
class SetterExpressionsTest extends FunSuite with CompilationTest {

  import com.avsystem.scex.validation.SymbolValidator._

  def applySetter[R: TypeTag, T: TypeTag](
    expr: String, root: R, value: T,
    acl: List[MemberAccessSpec] = PredefinedAccessSpecs.basicOperations,
    header: String = "",
    template: Boolean = false) = {

    val setterExpression = compiler.getCompiledSetterExpression[SimpleContext[R], T](
      createProfile(acl), expr, template, header = header)
    setterExpression.apply(SimpleContext(root)).apply(value)
  }

  test("scala variable setter test") {
    val target = new SetterTarget
    applySetter("costam", target, 42, allow(on { st: SetterTarget => st.all.introduced.members }))
    assert(42 == target.costam)
  }

  test("scala setter test") {
    val target = new SetterTarget
    applySetter("lol", target, 42, allow(on { st: SetterTarget => st.all.introduced.members }))
    assert(42 == target.lol)
  }

  test("direct bean setter test") {
    val target = new JavaSetterTarget
    applySetter("getBeanprop", target, 42, allow(on { st: JavaSetterTarget => st.all.introduced.members }))
    assert(42 == target.getBeanprop)
  }

  test("adapted root bean setter test") {
    val target = new JavaSetterTarget
    applySetter("beanprop", target, 42, allow(on { st: JavaSetterTarget => st.all.introduced.members }))
    assert(42 == target.getBeanprop)
  }

  test("adapted root boolean bean setter test") {
    val target = new JavaSetterTarget
    applySetter("awesome", target, true, allow(on { st: JavaSetterTarget => st.all.introduced.members }))
    assert(target.isAwesome)
  }

  test("adapted root bean setter test with conversion") {
    val target = new JavaSetterTarget
    val header = "import SetterConversions._"
    applySetter("awesome", target, "true", allow(on { st: JavaSetterTarget => st.all.introduced.members }), header = header)
    assert(target.isAwesome)
  }

  test("adapted root bean setter template test with conversion") {
    val target = new JavaSetterTarget
    val header = "import SetterConversions._"
    val acl = PredefinedAccessSpecs.basicOperations ++ allow {
      on { st: JavaSetterTarget => st.all.introduced.members }
    }
    applySetter("${awesome}", target, "true", acl, template = true, header = header)
    assert(target.isAwesome)
  }

  test("adapted root bean setter template test with conversion and splicer") {
    val target = new JavaSetterTarget
    val header = "import SetterConversions._; import CustomBooleanSplicer._"
    val acl = PredefinedAccessSpecs.basicOperations ++ allow {
      CustomBooleanSplicer.jBooleanSplicer: @silent
      on { s: Splicer[JBoolean] => s.toString(_: JBoolean) }
      on { st: JavaSetterTarget => st.all.introduced.members }
    }
    applySetter("${awesome}", target, "true", acl, template = true, header = header)
    assert(target.isAwesome)
  }

  test("adapted non-root bean setter test") {
    val target = new JavaSetterTarget
    applySetter("self.beanprop", target, 42, allow(on { st: JavaSetterTarget => st.all.introduced.members }))
    assert(42 == target.self.getBeanprop)
  }

  test("adapted non-root template bean setter test") {
    val target = new JavaSetterTarget
    applySetter("${self.beanprop}", target, 42, allow(on { st: JavaSetterTarget => st.all.introduced.members }), template = true)
    assert(42 == target.self.getBeanprop)
  }

  test("adapted non-root boolean bean setter test") {
    val target = new JavaSetterTarget
    applySetter("self.awesome", target, true, allow(on { st: JavaSetterTarget => st.all.introduced.members }))
    assert(target.isAwesome)
  }

  test("java field test") {
    val target = new JavaSetterTarget
    applySetter("field", target, 42, allow(on { st: JavaSetterTarget => st.all.introduced.members }))
    assert(42 == target.field)
  }

  test("dynamic variable setter test") {
    val setterExpression = compiler.getCompiledSetterExpression[SimpleContext[Unit], String](
      createProfile(Nil), "_vars.lol", template = false)

    val context = SimpleContext(())
    setterExpression.apply(context).apply("42")
    assert("42" == context.getVariable("lol"))
  }

  test("typed dynamic variable setter test") {
    import scala.reflect.runtime.universe.typeOf

    val setterExpression = compiler.getCompiledSetterExpression[SimpleContext[Unit], Int](
      createProfile(Nil), "_vars.lol", template = false, variableTypes = Map("lol" -> typeOf[Int]))

    val context = SimpleContext(())
    setterExpression.apply(context).apply(42)
    assert(42 == context.getTypedVariable[Int]("lol"))
  }

  test("accepted type reporting test") {
    val acl = allow(on { st: JavaSetterTarget => st.all.introduced.members })
    val setterExpression = compiler.getCompiledSetterExpression[SimpleContext[JavaSetterTarget], Nothing](
      createProfile(acl), "beanprop", template = false
    )
    val setter = setterExpression.apply(SimpleContext(new JavaSetterTarget))
    assert(setter.acceptedType == Type("Int", classOf[Int]))
  }

}
