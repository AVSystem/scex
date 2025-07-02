import java.{lang => jl, util => ju}
import com.avsystem.scex.util.Literal

import scala.annotation.nowarn

/**
 * Created: 30-05-2014
 * Author: ghik
 */
object AutoConvertTest {

  implicit final class stringAutoConvert(private val str: String) extends AnyVal {
    @nowarn("msg=Implicit parameters")
    def autoConvert[T](implicit conv: Literal => T): T =
      conv(Literal(str))
  }

  def main(args: Array[String]): Unit = {
    val int: Int = "123".autoConvert
    val double: Double = "123.5".autoConvert
    println(int)
    println(123 + "22".toByte)
  }
}
