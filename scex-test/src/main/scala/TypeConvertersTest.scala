import java.{lang => jl, util => ju}


object TypeConvertersTest {

  import scala.language.existentials

  def main(args: Array[String]) {
    val clazz = classOf[TypedLol[T]#Dafuq[F] forSome {type T; type F}]

    import com.avsystem.scex.compiler.JavaTypeParsing._

    println(javaTypeAsScalaType(clazz))
    println(javaTypeAsScalaType(classOf[JavaLol#InnerLol]))
    println(typeVariableDeclarations(classToExistential(clazz).typeVars))
  }
}
