
import com.avsystem.scex.TestMacros
import java.{util => ju, lang => jl}
import scala.language.experimental.macros

object Playground {

  def main(args: Array[String]) {
    val (treeCreator, typeCreator) = TestMacros.gimme(None.toString)

    import scala.reflect.runtime.universe._

    val tree = Expr[Any](rootMirror, treeCreator)(TypeTag[Any](rootMirror, typeCreator)).tree

    tree.foreach { t =>
      println(s"${t.symbol}  ${t.tpe} -- ${showRaw(t)}")
    }
  }

}
