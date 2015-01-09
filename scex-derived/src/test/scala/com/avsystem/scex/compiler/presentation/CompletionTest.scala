package com.avsystem.scex.compiler.presentation

import com.avsystem.scex.Type
import com.avsystem.scex.compiler.presentation.ScexPresentationCompiler.{Member, Param}

import scala.reflect.runtime.{universe => ru}

/**
 * Author: ghik
 * Created: 11/18/14.
 */
trait CompletionTest {
  protected def scexType[T: ru.TypeTag] = {
    val tag = ru.typeTag[T]
    Type(tag.tpe.toString, tag.mirror.runtimeClass(tag.tpe.erasure))
  }

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
