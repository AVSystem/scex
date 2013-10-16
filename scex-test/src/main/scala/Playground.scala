
import com.avsystem.scex.japi.XmlFriendlyJavaScexCompiler
import java.{util => ju, lang => jl}

object Playground {
  def main(args: Array[String]) {
    import scala.reflect.runtime.universe._

    val t = typeOf[XmlFriendlyJavaScexCompiler]
    t.baseClasses foreach println
  }
}
