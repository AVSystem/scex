import com.avsystem.scex.compiler.JavaTypeParsing
import java.{util => ju, lang => jl}


object TypeConvertersTest {

  import scala.language.existentials

  def main(args: Array[String]) {
    val clazz = classOf[TypedLol[T]#Dafuq[F] forSome {type T; type F}]

    import JavaTypeParsing._

    println(javaTypeAsScalaType(clazz))
    println(javaTypeAsScalaType(classOf[JavaLol#InnerLol]))
    println(appliedBoundedTypes(classToExistential(clazz).typeVars))
  }
}
