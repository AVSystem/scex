import java.{lang => jl, util => ju}

import com.avsystem.scex.util.Literal

/**
 * Created: 30-05-2014
 * Author: ghik
 */
object AutoConvertTest {

  implicit class stringAutoConvert(val str: String) extends AnyVal {
    def autoConvert[T](implicit conv: Literal => T): T =
      conv(Literal(str))
  }

  def main(args: Array[String]) {
    val int: Int = "123".autoConvert
    val double: Double = "123.5".autoConvert
    println(int)
    println(123 + "22".toByte)
  }

}
