package com.avsystem.scex.util

import java.io.ByteArrayOutputStream

object EscapedBytes {
  def render(bytes: Array[Byte]): String = {
    val sb = new StringBuilder
    bytes.foreach {
      case '\\' => sb ++= "\\\\"
      case b if b > 0x1F && b < 0x7F => sb += b.toChar
      case b => sb ++= f"\\x$b%02x"
    }
    sb.result()
  }

  def parse(repr: String): Array[Byte] = {
    val baos = new ByteArrayOutputStream
    def loop(it: Iterator[Char]): Unit = if (it.hasNext) {
      it.next() match {
        case '\\' => it.next() match {
          case 'x' =>
            def readDigit(): Int = if (it.hasNext) it.next() match {
              case d if d >= '0' && d <= '9' => d - '0'
              case d if d >= 'A' && d <= 'F' => d - 'A' + 10
              case d if d >= 'a' && d <= 'f' => d - 'a' + 10
              case c => throw new IllegalArgumentException(s"Expected hex digit, got character: $c")
            } else throw new IllegalArgumentException(s"Expected hex digit, got end of string")
            var byte = readDigit() * 16
            byte += readDigit()
            baos.write(byte)
          case '\\' => baos.write('\\')
          case c =>
            throw new IllegalArgumentException(s"Invalid escape character: $c, only \\ and hex escapes are allowed")
        }
        case c if c > 0x1F && c < 0x7F =>
          baos.write(c)
        case c =>
          throw new IllegalArgumentException(s"Invalid character in binary representation: $c")
      }
      loop(it)
    }
    loop(repr.iterator)
    baos.toByteArray
  }
}

