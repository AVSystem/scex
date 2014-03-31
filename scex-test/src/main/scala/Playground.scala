
import com.avsystem.scex.compiler.TemplateInterpolations
import java.{util => ju, lang => jl}
import scala.language.experimental.macros

object Playground {

  def main(args: Array[String]) {
    import scala.reflect.runtime.universe._

    println(showRaw(typeOf[TemplateInterpolations.Splicer[String]]))
  }

}
