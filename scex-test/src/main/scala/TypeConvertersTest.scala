import com.avsystem.scex.compiler.JavaTypeParsing
import com.avsystem.scex.util.CommonUtils
import java.{util => ju, lang => jl}
import scala.reflect.runtime.{universe => ru}

object TypeConvertersTest {

  import scala.language.existentials

  def main(args: Array[String]) {
    val clazz = classOf[TypedLol[T]#Dafuq[F] forSome {type T; type F}]

    new JavaTypeParsing {
      println(javaTypeAsScalaType(clazz))
      println(javaTypeAsScalaType(classOf[JavaLol#InnerLol]))
      println(boundedTypeVariables(classToExistential(clazz).typeVars))
    }

    CommonUtils.hierarchy(classOf[ju.ArrayList[_]]) foreach println
  }
}
