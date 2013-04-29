import com.avsystem.scex.Utils
import java.{util => ju, lang => jl}
import scala.collection.mutable
import scala.collection.JavaConverters._
import scala.reflect.runtime.{universe => ru}

object TypeConvertersTest {

  import com.avsystem.scex.TypeConverters._
  import scala.language.existentials

  def main(args: Array[String]) {
    val clazz = classOf[TypedLol[T]#Dafuq[F] forSome {type T; type F}]

    println(javaTypeAsScalaType(clazz))
    println(javaTypeAsScalaType(classOf[JavaLol#InnerLol]))
    println(boundedTypeVariables(classToExistential(clazz).typeVars))

    Utils.hierarchy(classOf[ju.ArrayList[_]]) foreach println
  }
}
