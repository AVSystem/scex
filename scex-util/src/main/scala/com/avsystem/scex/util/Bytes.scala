package com.avsystem.scex.util

import java.io.UnsupportedEncodingException
import java.nio.charset.StandardCharsets

import com.avsystem.commons.jiop.JavaInterop._
import com.avsystem.scex.presentation.annotation.Documentation
import org.apache.commons.codec.binary.{Base64, Hex}

import scala.collection.mutable
import scala.util.hashing.MurmurHash3

final class Bytes(val bytes: Array[Byte]) extends Comparable[Bytes] {
  override def hashCode(): Int = MurmurHash3.bytesHash(bytes)
  override def equals(other: Any): Boolean = other match {
    case b: Bytes => java.util.Arrays.equals(bytes, b.bytes)
    case _ => false
  }
  override def toString: String = s"Bytes($escaped)"
  override def compareTo(o: Bytes): Int = {
    def loop(i: Int): Int =
      if (i == bytes.length && i == o.bytes.length) 0
      else if (i == bytes.length) -1
      else if (i == o.bytes.length) 1
      else {
        val b1 = bytes(i)
        val b2 = o.bytes(i)
        if (b1 == b2) loop(i + 1) else b1 - b2
      }
    loop(0)
  }

  @Documentation("Encodes this sequence of bytes as string with non-ASCII bytes and backslash escaped, e.g. 'hsg\\x7c\\x0dfoo\\\\bar'")
  def escaped: String = EscapedBytes.render(bytes)

  @Documentation("Encodes this sequence of bytes as hexadecimal string")
  def hex: String = Hex.encodeHexString(bytes)

  @Documentation("Encodes this sequence of bytes as BASE64 string")
  def base64: String = Base64.encodeBase64String(bytes)

  @Documentation("Decodes this sequence of bytes as UTF-8 string")
  def decodeUTF8: String = new String(bytes, StandardCharsets.UTF_8)

  @Documentation("Decodes this sequence of bytes as string using given charset")
  def decode(charset: String): String =
    try new String(bytes, charset) catch {
      case e: UnsupportedEncodingException => throw new IllegalArgumentException(e)
    }

  def asList: JList[Byte] = new mutable.WrappedArray.ofByte(bytes).asJava
}
