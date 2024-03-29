package com.avsystem.scex.util

import java.io.UnsupportedEncodingException
import java.nio.charset.StandardCharsets
import java.text.{ParseException, SimpleDateFormat}
import java.util.regex.Pattern
import java.util.{Locale, TimeZone}

import com.avsystem.commons.jiop.JavaInterop._
import com.avsystem.scex.presentation.annotation.Documentation
import com.google.common.base.Splitter
import com.google.common.collect.Lists
import org.apache.commons.codec.DecoderException
import org.apache.commons.codec.binary.{Base64, Hex}
import org.apache.commons.codec.digest.{DigestUtils, HmacAlgorithms, HmacUtils}
import org.apache.commons.lang3.StringUtils

final class StringMiscOps(private val wrapped: String) extends AnyVal {
  @Documentation("Converts this string to date assuming it is a date in the <tt>yyyy.MM.dd HH:mm:ss</tt> format " +
    "or a number of milliseconds since 1970.01.01 00:00:00.")
  def toDate: JDate = try CommonDateFormat.get.parse(wrapped) catch {
    case _: ParseException => try new JDate(wrapped.toLong) catch {
      case _: NumberFormatException => throw new IllegalArgumentException(s"Cannot parse date: $wrapped")
    }
  }

  @Documentation("Converts this string to date by parsing it according to the specified `format` " +
    "defined with Java SimpleDateFormat. " +
    "Example format: <tt>yyyy.MM.dd HH:mm:ss</tt>.")
  def toDate(format: String): JDate = try new SimpleDateFormat(format).parse(wrapped) catch {
    case _: ParseException =>
      throw new IllegalArgumentException(s"Cannot parse date $wrapped using format $format")
  }

  @Documentation("Converts this string to date by parsing it according to specified date `format` and `timezone`. " +
    "Example format: <tt>yyyy.MM.dd HH:mm:ss</tt>. Example timeZone: <tt>GMT+2</tt>. " +
    "Date format as per Java SimpleDateFormat.")
  def toDate(format: String, timeZone: String): JDate = try {
    val formatter = new SimpleDateFormat(format)
    formatter.setTimeZone(TimeZone.getTimeZone(timeZone))
    formatter.parse(wrapped)
  } catch {
    case e: ParseException =>
      throw new IllegalArgumentException(e)
  }

  @Documentation("Splits this string into a list of strings using given separator. " +
    "Resulting parts are not whitespace-trimmed and blank parts are not filtered out.")
  def splitBy(separator: String): JList[String] = java.util.Arrays.asList(wrapped.split(Pattern.quote(separator)): _*)

  @Documentation("Splits this string into a list of strings using given separator. " +
    "Resulting parts are whitespace-trimmed and blank parts are returned as empty strings.")
  def splitByTrim(separator: String): JList[String] = Lists.newArrayList(Splitter.on(separator).trimResults.split(wrapped))

  @Documentation("Assumes this string is a hexadecimal number and converts it to a decimal number.")
  def parseHex: Long = java.lang.Long.parseLong(wrapped, 16)

  @Documentation("Prepends this string with given `padding`, repeated if needed. Number of characters in the output corresponds to `desiredLength`.")
  def leftPad(desiredLength: Int, padding: String): String =
    StringUtils.leftPad(wrapped, desiredLength, padding)

  @Documentation("Appends given `padding` to this string, repeated if needed. Number of characters in the output corresponds to `desiredLength`.")
  def rightPad(desiredLength: Int, padding: String): String =
    StringUtils.rightPad(wrapped, desiredLength, padding)

  @Documentation("Ensures that this string starts with given prefix by prepending it to this string if needed.")
  def ensureStart(prefix: String): String =
    if (wrapped.startsWith(prefix)) wrapped else prefix + wrapped

  @Documentation("Ensures that this string ends with given suffix by appending it to this string if needed.")
  def ensureEnd(postfix: String): String =
    if (wrapped.endsWith(postfix)) wrapped else wrapped + postfix

  @Documentation("Decodes this string assuming it contains escaped arbitrary bytes (backslash and non-ASCII bytes are escaped).")
  def decodeEscapedBytes = new Bytes(EscapedBytes.parse(wrapped))

  @Documentation("Assumes this string is a BASE64 encoding of another string and decodes it using UTF-8.")
  def decodeBase64String = new String(Base64.decodeBase64(wrapped), StandardCharsets.UTF_8)

  @Documentation("Decodes this string assuming it contains BASE64 encoding of some arbitrary bytes.")
  def decodeBase64 = new Bytes(Base64.decodeBase64(wrapped))

  @Documentation("Encodes this string's UTF-8 representation as BASE64.")
  def encodeBase64: String = Base64.encodeBase64String(wrapped.getBytes(StandardCharsets.UTF_8))

  @Documentation("Encodes this string's UTF-8 representation as hexadecimal.")
  def encodeHex: String = Hex.encodeHexString(wrapped.getBytes(StandardCharsets.UTF_8))

  @Documentation("Decodes this string assuming it contains hexadecimal encoding of UTF-8 bytes of another string.")
  def decodeHexString: String = try new String(Hex.decodeHex(wrapped.toCharArray), StandardCharsets.UTF_8) catch {
    case e: DecoderException =>
      throw new IllegalArgumentException(e)
  }

  @Documentation("Decodes this string assuming it contains hexadecimal encoding of some arbitrary bytes.")
  def decodeHex: Bytes = try new Bytes(Hex.decodeHex(wrapped.toCharArray)) catch {
    case e: DecoderException =>
      throw new IllegalArgumentException(e)
  }

  @Documentation("Encodes this string into bytes using UTF-8.")
  def encodeUTF8 = new Bytes(wrapped.getBytes(StandardCharsets.UTF_8))

  @Documentation("Encodes this string into bytes using given charset.")
  def encode(charset: String): Bytes = try
    new Bytes(wrapped.getBytes(charset))
  catch {
    case e: UnsupportedEncodingException =>
      throw new IllegalArgumentException(e)
  }

  @Documentation("Calculates MD5 digest from contents of this string encoded using UTF-8. " +
    "The output is a 32 character hexadecimal string.")
  def md5Hex: String = DigestUtils.md5Hex(wrapped.getBytes(StandardCharsets.UTF_8))

  @Documentation("Calculates HMAC MD5 digest from contents of this string. " +
    "The output is a 32 character hexadecimal string.")
  def hmacMD5(key: String): String = new HmacUtils(HmacAlgorithms.HMAC_MD5, key).hmacHex(wrapped)

  @Documentation("Returns this string in lowercase.")
  def toLowerCaseEnglish: String = wrapped.toLowerCase(Locale.ENGLISH)
  @Documentation("Returns this string in uppercase.")
  def toUpperCaseEnglish: String = wrapped.toUpperCase(Locale.ENGLISH)
}
