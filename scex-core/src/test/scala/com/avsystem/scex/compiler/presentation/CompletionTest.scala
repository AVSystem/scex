package com.avsystem.scex.compiler.presentation

import com.avsystem.commons.misc.TypeString
import com.avsystem.scex.Type
import com.avsystem.scex.compiler.presentation.ScexPresentationCompiler.{Member, Param}

import scala.reflect.{ClassTag, classTag}

/**
  * Author: ghik
  * Created: 11/18/14.
  */
trait CompletionTest {
  protected def scexType[T: TypeString : ClassTag]: Type =
    Type(TypeString.of[T], classTag[T].runtimeClass)

  case class PartialMember(
    name: String,
    returnType: Type,
    params: List[List[Param]] = Nil,
    implicitParams: List[Param] = Nil,
    doc: String = null)

  def asPartial(member: Member) = PartialMember(
    member.name,
    member.returnType,
    member.params,
    member.implicitParams,
    member.documentation.orNull)

}
