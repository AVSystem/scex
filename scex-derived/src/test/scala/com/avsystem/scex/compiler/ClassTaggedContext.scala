package com.avsystem.scex.compiler

import com.avsystem.scex.ExpressionContext

import scala.collection.mutable
import scala.reflect.ClassTag

/**
  * Author: ghik
  * Created: 22/10/15.
  */
class ClassTaggedContext extends ExpressionContext[Unit, Unit] {
  type VarTag[T] = ClassTag[T]

  val typedVars = new mutable.HashMap[(String, Class[_]), Any]

  def root: Unit = ()

  def getTypedVariable[T](name: String)(implicit tag: ClassTag[T]): T =
    typedVars((name, tag.runtimeClass)).asInstanceOf[T]

  def setTypedVariable[T](name: String, value: T)(implicit tag: ClassTag[T]): Unit =
    typedVars((name, tag.runtimeClass)) = value

  def getVariable(name: String): Unit = ()
  def setVariable(name: String, value: Unit): Unit = ()
}
