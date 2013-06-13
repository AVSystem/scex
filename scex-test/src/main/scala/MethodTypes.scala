import java.{util => ju, lang => jl}
import scala.reflect.runtime.universe._

object MethodTypes {

  class A {
    def a(cos: Any)(cosinnego: Int): Int = 5

    def b(cos: Int)(cosinnego: Int): String = ""

    def c = 5
  }

  class G[T] {
    def id(t: T): T = t
  }

  class C extends G[Int]

  implicit class IA(a: A) {
    def a(cos: String): String = "jfkdjdfk"
  }

  def methodTypesMatch(originalTpe: Type, implicitTpe: Type): Boolean = {
    def paramsMatch(origParams: List[Symbol], implParams: List[Symbol]): Boolean =
      (origParams, implParams) match {
        case (origHead :: origTail, implHead :: implTail) =>
          implHead.typeSignature <:< origHead.typeSignature && paramsMatch(origTail, implTail)
        case (Nil, Nil) => true
        case _ => false
      }

    (originalTpe, implicitTpe) match {
      case (MethodType(origParams, origResultType), MethodType(implParams, implResultType)) =>
        paramsMatch(origParams, implParams) && methodTypesMatch(origResultType, implResultType)
      case (MethodType(_, _), _) | (_, MethodType(_, _)) => false
      case (_, _) => true
    }
  }

  def typeSignatureOf[T: TypeTag](member: String) =
    typeOf[T].member(newTermName(member)).typeSignature

  def main(args: Array[String]) {
    println(methodTypesMatch(typeSignatureOf[A]("a"), typeSignatureOf[A]("b")))
  }
}
