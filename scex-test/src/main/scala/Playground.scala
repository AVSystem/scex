
import java.{util => ju, lang => jl}
import scala.beans.BeanProperty
import scala.collection.mutable
import scala.reflect.runtime.{universe => ru}

/**
 * Created with IntelliJ IDEA.
 * User: ghik
 * Date: 25.01.13
 * Time: 20:40
 */
object Playground {
  def main(args: Array[String]) {
  }

  private def generateAdapter(clazz: Class[_]) = {
    val classSymbol = ru.runtimeMirror(clazz.getClassLoader).classSymbol(clazz)
    val tpe = classSymbol.toType

    val keywords = List("class")

    val adapterTemplate = """
                            |implicit class %s_Adapter(val wrapped: %s) extends AnyVal with com.avsystem.scex.compiler.JavaGetterAdapter {
                            |%s
                            |}
                          """.stripMargin

    val getterPattern = "get([A-Z][a-z0-9]*)+"
    def propNameFromGetter(getter: String) = getter(3).toLower + getter.substring(4)

    val propsWithGetters = tpe.members.withFilter {
      member =>
        val name = member.name.decoded
        member.isMethod &&
          member.isPublic &&
          member.asMethod.paramss == List(List()) &&
          name.matches(getterPattern) &&
          !keywords.contains(propNameFromGetter(name))
    } map {
      member =>
        val name = member.name.decoded
        (propNameFromGetter(name), name)
    }

    if (propsWithGetters.nonEmpty) {
      val sb = new StringBuilder
      propsWithGetters foreach {
        case (prop, getter) =>
          sb ++= s"    def $prop = wrapped.$getter\n"
      }
      val propDefs = sb.mkString

      val typeName = tpe.erasure.toString
      val wrapperClassName = classSymbol.fullName.replaceAll("\\.", "_")

      Some(adapterTemplate.format(wrapperClassName, typeName, propDefs))
    } else {
      None
    }
  }
}