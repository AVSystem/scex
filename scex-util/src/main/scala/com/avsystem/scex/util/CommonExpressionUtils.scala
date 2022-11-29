package com.avsystem.scex.util

import java.text.ParseException
import java.util.Date

import com.avsystem.commons._
import com.avsystem.scex.SetterConversion
import com.avsystem.scex.compiler.TemplateInterpolations.Splicer
import com.avsystem.scex.presentation.annotation.Documentation
import com.avsystem.scex.util.function._

/**
  * Author: ghik
  * Created: 16/10/15.
  */
object CommonExpressionUtils {
  // a replacement for safe dereference operator (?.) and elvis operator (?:)
  implicit class any2qmark[A](value: => A) {
    def ?[B >: A](default: => B): B = {
      val result = try value.opt catch {
        case _: NullPointerException |
             _: NoSuchElementException |
             _: UnsupportedOperationException |
             _: IndexOutOfBoundsException |
             _: NumberFormatException |
             _: IllegalArgumentException |
             _: ParseException |
             _: ExpressionRecoverableException => Opt.Empty
      }
      result getOrElse default
    }
  }

  implicit class exprUniversalOps[A](private val a: A) extends AnyVal {
    def useAs[B](f: A => B): B = f(a)
  }

  implicit class charOps(private val c: Char) extends AnyVal {
    def isHexDigit: Boolean =
      c.isDigit || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f')
  }

  implicit def enrichArray[T <: AnyRef](array: Array[T]): EnrichedArray[T] =
    new EnrichedArray(array)

  implicit def enrichDate(date: JDate): EnrichedDate =
    new EnrichedDate(date)

  implicit def stringNetworkOps(str: String): StringNetworkOps =
    new StringNetworkOps(str)

  implicit def stringMiscOps(str: String): StringMiscOps =
    new StringMiscOps(str)

  implicit def literalToStringList(literal: Literal): JList[String] =
    literal.literalString.splitBy(",")

  implicit def dateToOrdered(date: Date): Ordered[Date] =
    Ordered.orderingToOrdered(date)

  implicit def literalToDate(literal: Literal): Date =
    literal.literalString.toDate

  implicit def byteArrayToString(byteArray: Array[Byte]): String =
    new String(byteArray)

  implicit object dateSplicer extends Splicer[JDate] {
    def toString(date: Date) =
      if (date != null) date.format else null
  }

  implicit object collectionSplicer extends Splicer[JSet[String]] {
    override def toString(col: JSet[String]): String =
      col.asScala.mkString(",")
  }

  // deprecated, exist for backwards compatibility of expressions
  @Documentation("String utility functions")
  val string: StringUtil = StringUtilImpl.INSTANCE

  @Documentation("Network utility functions")
  val net: NetUtil = NetUtilImpl.INSTANCE

  @Documentation("Formatting utility functions")
  val format: FormatUtil = FormatUtilImpl.INSTANCE

  @Documentation("Date utility functions")
  val date: DateUtil = DateUtilImpl.INSTANCE

  @Documentation("Mathematical functions")
  val math = scala.math.`package`

  @Documentation("Access context variables")
  def getVariable(name: String)(implicit ctx: SimpleContext[_]): String = {
    ctx.getVariable(name)
  }

  def bytes(bs: Byte*): Bytes = new Bytes(bs.toArray)

  implicit class byteListOps(private val l: JList[Byte]) extends AnyVal {
    def toBytes: Bytes = new Bytes(l.asScala.toArray)
  }

  implicit val stringToBooleanOnSetter: SetterConversion[String, JBoolean] =
    SetterConversion(_.toBoolean)

  implicit val stringToDateOnSetter: SetterConversion[String, JDate] =
    SetterConversion(CommonDateFormat.get.parse(_))
}
