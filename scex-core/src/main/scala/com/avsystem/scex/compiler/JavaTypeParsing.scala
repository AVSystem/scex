package com.avsystem.scex
package compiler

import java.lang.reflect._
import java.lang.{reflect => jlr}
import java.{util => ju, lang => jl}
import scala.Array
import scala.Some
import scala.collection.mutable.ListBuffer
import scala.language.existentials

/**
 * Utils for conversions of Java types into string representations of their Scala counterparts.
 */
object JavaTypeParsing {

  case class ScalaTypeVariable(name: String, upperBounds: Array[Type], lowerBounds: Array[Type]) extends Type

  case class RawClass(clazz: Class[_]) extends Type

  case class ExistentialType(polyType: Type, typeVars: List[Type]) extends Type

  case class WrappedParameterizedType(rawType: Type, ownerType: Type, typeArgs: Array[Type]) extends Type

  case object TypeAny extends Type

  case object TypeAnyVal extends Type

  case object TypeAnyRef extends Type

  case object TypeNull extends Type

  case object TypeNothing extends Type

  // extractors for java.lang.reflect.Type subinterfaces
  object WildcardType {
    def unapply(tpe: WildcardType) =
      Some((tpe.getUpperBounds, tpe.getLowerBounds))
  }

  object TypeVariable {
    def unapply(tv: Any) = tv match {
      case tv: TypeVariable[_] => Some((tv.getName, tv.getBounds, Array.empty[Type]))
      case ScalaTypeVariable(name, upperBounds, lowerBounds) => Some((name, upperBounds, lowerBounds))
      case _ => None
    }
  }

  object ParameterizedType {
    def unapply(ptpe: ParameterizedType) =
      Some((ptpe.getRawType, ptpe.getOwnerType, ptpe.getActualTypeArguments))
  }

  object GenericArrayType {
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

  def javaTypeAsScalaType(tpe: Type): String = tpe match {
    case clazz: Class[_] =>
      javaTypeAsScalaType(classToExistential(clazz))

    case WildcardType(upperBounds, lowerBounds) =>
      "_" + bounds(upperBounds, lowerBounds)

    case TypeVariable(name, _, _) =>
      name

    case WrappedParameterizedType(rawType, ownerType, typeArguments) =>
      val clazz = rawType.asInstanceOf[Class[_]]
      val typeArgumentsRepr = if (typeArguments.nonEmpty) typeArguments.map(javaTypeAsScalaType).mkString("[", ", ", "]") else ""

      if (ownerType != null) {
        javaTypeAsScalaType(ownerType) + "#" + clazz.getSimpleName + typeArgumentsRepr
      } else {
        javaTypeAsScalaType(RawClass(clazz)) + typeArgumentsRepr
      }

    case paramType: ParameterizedType =>
      javaTypeAsScalaType(parameterizedTypeToExistential(paramType))

    case GenericArrayType(componentType) =>
      s"Array[${javaTypeAsScalaType(componentType)}]"

    case ExistentialType(polyType, typeVars) =>
      def typeVarDefs =
        if (typeVars.nonEmpty)
          typeVariableDeclarations(typeVars).map(tv => s"type $tv").mkString(" forSome {", "; ", "}")
        else ""

      javaTypeAsScalaType(polyType) + (if (typeVars.nonEmpty) typeVarDefs else "")

    case RawClass(arrayClazz) if arrayClazz.isArray =>
      s"Array[${javaTypeAsScalaType(arrayClazz.getComponentType)}]"

    case RawClass(clazz) if clazz.isPrimitive =>
      primitiveTypes(clazz)

    case RawClass(clazz) if !clazz.isAnonymousClass && !clazz.isSynthetic =>
      clazz.getCanonicalName

    case TypeAny =>
      "Any"

    case TypeAnyVal =>
      "AnyVal"

    case TypeAnyRef =>
      "AnyRef"

    case TypeNull =>
      "Null"

    case TypeNothing =>
      "Nothing"

    case _ =>
      throw new IllegalArgumentException(s"Cannot convert $tpe into Scala type")
  }

  def erasureOf(tpe: Type): Class[_] = tpe match {
    case clazz: Class[_] =>
      clazz
    case RawClass(clazz) =>
      clazz
    case GenericArrayType(componentType) =>
      jlr.Array.newInstance(erasureOf(componentType), 0).getClass
    case ParameterizedType(rawType, _, _) =>
      erasureOf(rawType)
    case _ =>
      throw new IllegalArgumentException(s"Type $tpe does not have erasure")
  }

  /**
   * Example: transforms java type <tt>A<? extends B, C>.D<? super E></tt> into Scala type
   * <tt>A[T1,C]#D[T2] forSome {type T1 <: B; type T2 >: E}</tt>
   * @param paramType
   * @return
   */
  def parameterizedTypeToExistential(paramType: ParameterizedType): ExistentialType = {
    var i = 0
    def newTypeVarName() = {
      i += 1
      s"T$i"
    }

    // example: transforms type arguments <? extends A, ? super C> into [T1, T2] forSome {type T1 <: A; type T2 >: C}
    def transformTypeArgs(typeArgs: Array[Type]): (Array[Type], List[Type]) = {
      val typeVariablesBuffer = new ListBuffer[Type]

      // a little ugly: relying on side effects inside map
      val transformedArgs = typeArgs.map {
        case wc: WildcardType =>
          val newTypeVariable = ScalaTypeVariable(newTypeVarName(), wc.getUpperBounds, wc.getLowerBounds)
          typeVariablesBuffer += newTypeVariable
          newTypeVariable
        case tpe: Type =>
          tpe
      }

      (transformedArgs, typeVariablesBuffer.result())
    }

    def parameterizedTypeToExistentialIn(paramType: ParameterizedType): ExistentialType = {
      val ParameterizedType(rawType, ownerType, typeArgs) = paramType
      val ExistentialType(newOwnerType, ownerTypeVariables) = ownerType match {
        case ownerType: ParameterizedType => parameterizedTypeToExistentialIn(ownerType)
        case _ => ExistentialType(null, Nil)
      }

      val (newTypeArgs, typeVariables) = transformTypeArgs(typeArgs)

      ExistentialType(WrappedParameterizedType(rawType, newOwnerType, newTypeArgs), ownerTypeVariables ::: typeVariables)
    }

    parameterizedTypeToExistentialIn(paramType)
  }

  // lifts raw class into an existential type, e.g. java.util.List becomes java.util.List[T] forSome {type T}
  def classToExistential(clazz: Class[_]): ExistentialType = {
    val enclosingClass = clazz.getEnclosingClass

    val ExistentialType(ownerType, ownerTypeVariables) =
      if (enclosingClass != null && !isStatic(clazz))
        classToExistential(enclosingClass)
      else
        ExistentialType(null, Nil)

    val typeVariables = ownerTypeVariables ::: clazz.getTypeParameters.toList

    val typeArgs = clazz.getTypeParameters.asInstanceOf[Array[Type]]

    ExistentialType(WrappedParameterizedType(clazz, ownerType, typeArgs), typeVariables)
  }

  def bounds(upperBounds: Array[Type], lowerBounds: Array[Type]): String = {
    val upperBoundsRepr = upperBounds.filter(_ != classOf[Object]).map(javaTypeAsScalaType).mkString(" with ")
    val lowerBoundsRepr = lowerBounds.map(javaTypeAsScalaType).mkString(" with ")

    (if (upperBoundsRepr.nonEmpty) " <: " + upperBoundsRepr else "") +
      (if (lowerBoundsRepr.nonEmpty) " >: " + lowerBoundsRepr else "")
  }

  def typeVariableDeclarations(typeVars: List[Type]): List[String] =
    typeVars.map {
      case TypeVariable(name, upperBounds, lowerBounds) =>
        name + bounds(upperBounds, lowerBounds)
    }

  val stringSupertypes = Set(TypeAny, TypeAnyRef, classOf[Object], classOf[Serializable], classOf[CharSequence],
    WrappedParameterizedType(classOf[Comparable[_]], null, Array(classOf[String])), classOf[String]).map(javaTypeAsScalaType)
}
