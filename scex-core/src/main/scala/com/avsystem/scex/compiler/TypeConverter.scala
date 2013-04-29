package com.avsystem.scex.compiler

import java.lang.reflect.{Type, WildcardType, TypeVariable, ParameterizedType, Modifier, GenericArrayType}
import java.{util => ju, lang => jl}
import scala.collection.mutable
import scala.language.existentials
import scala.reflect.runtime.{universe => ru}
import com.avsystem.scex.TypeTag

object TypeConverter {

  case class TypeVariableRef(name: String) extends Type

  case class RawClass(clazz: Class[_]) extends Type

  case class ClassExistential(polyType: Type, typeVars: List[TypeVariable[_]]) extends Type

  // extractors for java.lang.reflect.Type subinterfaces
  private object WildcardType {
    def unapply(tpe: WildcardType) =
      Some((tpe.getUpperBounds, tpe.getLowerBounds))
  }

  private object TypeVariable {
    def unapply(tv: TypeVariable[_]) =
      Some((tv.getName, tv.getBounds))
  }

  private object ParameterizedType {
    def unapply(ptpe: ParameterizedType) =
      Some((ptpe.getRawType, ptpe.getOwnerType, ptpe.getActualTypeArguments))
  }

  private object GenericArrayType {
    def unapply(gat: GenericArrayType) =
      Some(gat.getGenericComponentType)
  }

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

      case TypeTag(underlyingType) =>
        javaTypeAsScalaTypeIn(underlyingType)

      case WildcardType(upperBounds, lowerBounds) =>
        val upperBoundsRepr = upperBounds.filter(_ != classOf[Object]).map(javaTypeAsScalaTypeIn).mkString(" with ")
        val lowerBoundsRepr = lowerBounds.map(javaTypeAsScalaTypeIn).mkString(" with ")

        "_" + (if (upperBoundsRepr.nonEmpty) (" >: " + upperBoundsRepr) else "") +
          (if (lowerBoundsRepr.nonEmpty) (" >: " + lowerBoundsRepr) else "")

      case typeVariable@TypeVariable(name, _) if alreadyConvertedVariables.contains(typeVariable) =>
        name

      case typeVariable@TypeVariable(name, bounds) =>
        alreadyConvertedVariables += typeVariable
        val boundsRepr = bounds.filter(_ != classOf[Object]).map(javaTypeAsScalaTypeIn).mkString(" with ")
        name + (if (boundsRepr.nonEmpty) (" <: " + boundsRepr) else "")

      case ParameterizedType(rawType, ownerType, typeArguments) =>
        val clazz = rawType.asInstanceOf[Class[_]]
        val typeArgumentsRepr = if (typeArguments.nonEmpty) typeArguments.map(javaTypeAsScalaTypeIn).mkString("[", ", ", "]") else ""

        if (ownerType != null) {
          javaTypeAsScalaTypeIn(ownerType) + (if (isStatic(clazz)) "." else "#") + clazz.getSimpleName + typeArgumentsRepr
        } else {
          javaTypeAsScalaTypeIn(RawClass(clazz)) + typeArgumentsRepr
        }

      case GenericArrayType(componentType) =>
        s"Array[${javaTypeAsScalaTypeIn(componentType)}]"

      case ClassExistential(polyType, typeVars) =>
        def typeVarDefs = typeVars.map(tv => s"type ${tv.getName}").mkString(" forSome {", "; ", "}")
        javaTypeAsScalaTypeIn(polyType) + (if (typeVars.nonEmpty) typeVarDefs else "")

      case RawClass(arrayClazz) if arrayClazz.isArray =>
        s"Array[${javaTypeAsScalaTypeIn(arrayClazz.getComponentType)}]"

      case RawClass(clazz) if clazz.isPrimitive =>
        primitiveTypes(clazz)

      case RawClass(clazz) if !clazz.isAnonymousClass && !clazz.isSynthetic =>
        clazz.getName

      case TypeVariableRef(name) =>
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
      else
        ClassExistential(enclosingClass, Nil)

    val typeVariables =
      ownerTypeVariables ::: List[TypeVariable[_]](clazz.getTypeParameters: _*)

    val typeArgs: Array[Type] =
      clazz.getTypeParameters.map(typeParam => TypeVariableRef(typeParam.getName))

    val tpe =
      new ParameterizedType {
        def getActualTypeArguments: Array[Type] = typeArgs

        def getRawType: Type = clazz

        def getOwnerType: Type = ownerType
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
