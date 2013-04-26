package com.avsystem.scex

import java.lang.reflect.{Type, WildcardType, TypeVariable, ParameterizedType, Modifier, GenericArrayType}
import java.{util => ju, lang => jl}
import scala.reflect.runtime.{universe => ru}
import scala.collection.mutable

object TypeConverters {

  case class TypeSkolem(name: String) extends Type

  case class RawClass(clazz: Class[_]) extends Type

  case class ClassExistential(polyType: Type, typeVars: List[TypeVariable[_]]) extends Type

  private def isStatic(clazz: Class[_]) =
    Modifier.isStatic(clazz.getModifiers)

  private val primitiveTypes: Map[Class[_], String] = Map(
    classOf[Unit] -> "Unit",
    classOf[Boolean] -> "Boolean",
    classOf[Byte] -> "Byte",
    classOf[Char] -> "Char",
    classOf[Short] -> "Short",
    classOf[Int] -> "Int",
    classOf[Long] -> "Long",
    classOf[Float] -> "Float",
    classOf[Double] -> "Double"
  )

  def javaTypeAsScalaType(tpe: Type): String = {
    // for recursive generic type definitions like "Class C[T <: C[T]]"
    val alreadyConvertedVariables = new mutable.HashSet[TypeVariable[_]]

    def javaTypeAsScalaTypeIn(tpe: Type): String = tpe match {
      case clazz: Class[_] =>
        javaTypeAsScalaTypeIn(classToExistential(clazz))

      case wildcard: WildcardType =>
        val upperBounds = wildcard.getUpperBounds.filter(_ != classOf[Object]).map(javaTypeAsScalaTypeIn).mkString(" with ")
        val lowerBounds = wildcard.getLowerBounds.map(javaTypeAsScalaTypeIn).mkString(" with ")
        "_" + (if (upperBounds.nonEmpty) (" >: " + upperBounds) else "") + (if (lowerBounds.nonEmpty) (" >: " + lowerBounds) else "")

      case typeVariable: TypeVariable[_] if alreadyConvertedVariables.contains(typeVariable) =>
        typeVariable.getName

      case typeVariable: TypeVariable[_] =>
        alreadyConvertedVariables += typeVariable
        val name = typeVariable.getName
        val bounds = typeVariable.getBounds.filter(_ != classOf[Object]).map(javaTypeAsScalaTypeIn).mkString(" with ")
        name + (if (bounds.nonEmpty) (" <: " + bounds) else "")

      case appliedType: ParameterizedType =>
        val clazz = appliedType.getRawType.asInstanceOf[Class[_]]
        val owner = if (appliedType.getOwnerType != null) javaTypeAsScalaTypeIn(appliedType.getOwnerType) else null
        val typeParams = appliedType.getActualTypeArguments.map(javaTypeAsScalaTypeIn).mkString("[", ", ", "]")

        if (owner != null) {
          owner + (if (isStatic(clazz)) "." else "#") + clazz.getSimpleName + typeParams
        } else {
          javaTypeAsScalaTypeIn(RawClass(clazz)) + typeParams
        }

      case genericArrayType: GenericArrayType =>
        s"Array[${javaTypeAsScalaTypeIn(genericArrayType.getGenericComponentType)}]"

      case ClassExistential(polyType, typeVars) =>
        def typeVarDefs = typeVars.map(tv => s"type ${tv.getName}").mkString(" forSome {", "; ", "}")
        javaTypeAsScalaTypeIn(polyType) + (if (typeVars.nonEmpty) typeVarDefs else "")

      case RawClass(arrayClazz) if arrayClazz.isArray =>
        s"Array[${javaTypeAsScalaTypeIn(arrayClazz.getComponentType)}]"

      case RawClass(clazz) if clazz.isPrimitive =>
        primitiveTypes(clazz)

      case RawClass(clazz) if !clazz.isAnonymousClass && !clazz.isSynthetic =>
        clazz.getName

      case TypeSkolem(name) =>
        name

      case _ =>
        throw new IllegalArgumentException(s"Cannot convert $tpe into Scala type")
    }

    javaTypeAsScalaTypeIn(tpe)
  }

  // lifts raw class into an existential type, e.g. java.util.List becomes java.util.List[T] forSome {type T}
  def classToExistential(clazz: Class[_]): ClassExistential = {
    val enclosingClass = clazz.getEnclosingClass
    val ClassExistential(ownerType, ownerTypeVariables) =
      if (enclosingClass != null && !isStatic(clazz))
        classToExistential(enclosingClass)
      else if (enclosingClass != null)
        ClassExistential(RawClass(enclosingClass), Nil)
      else
        ClassExistential(null, Nil)

    val typeVariables = ownerTypeVariables ::: List[TypeVariable[_]](clazz.getTypeParameters: _*)

    val tpe = if (typeVariables.nonEmpty) {
      val typeArgs: Array[Type] = clazz.getTypeParameters.map(typeParam => TypeSkolem(typeParam.getName))

      new ParameterizedType {
        def getActualTypeArguments: Array[Type] = typeArgs

        def getRawType: Type = clazz

        def getOwnerType: Type = ownerType
      }
    } else {
      RawClass(clazz)
    }

    ClassExistential(tpe, typeVariables)
  }

  def boundedTypeVariables(typeVars: List[TypeVariable[_]]): String = {
    if (typeVars.nonEmpty)
      typeVars.map(javaTypeAsScalaType).mkString("[", ", ", "]")
    else
      ""
  }


}
