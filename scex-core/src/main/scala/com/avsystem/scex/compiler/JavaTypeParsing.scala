package com.avsystem.scex.compiler

import com.avsystem.scex.TypeTag
import java.lang.reflect._
import java.lang.{reflect => jlr}
import java.{util => ju, lang => jl}
import scala.Array
import scala.Some
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.existentials


/**
 * Utils for conversions of Java types into string representations of their Scala counterparts.
 */
object JavaTypeParsing {

  trait BoundedType extends Type {
    def name: String

    def upperBounds: Array[Type]

    def lowerBounds: Array[Type]
  }

  case class WildcardBoundedType(wc: WildcardType) extends BoundedType {
    def name = "_"

    def upperBounds = wc.getUpperBounds

    def lowerBounds = wc.getLowerBounds
  }

  case class TypeVariableBoundedType(tv: TypeVariable[_ <: GenericDeclaration]) extends BoundedType {
    def name = tv.getName

    def upperBounds = tv.getBounds

    def lowerBounds = Array.empty
  }

  case class ScalaTypeVariable(name: String, upperBounds: Array[Type], lowerBounds: Array[Type]) extends BoundedType

  object BoundedType {
    def unapply(boundedType: BoundedType) =
      Some((boundedType.name, boundedType.upperBounds, boundedType.lowerBounds))
  }

  case class TypeVariableRef(name: String) extends Type

  case class RawClass(clazz: Class[_]) extends Type

  case class ExistentialType(polyType: Type, typeVars: List[BoundedType]) extends Type

  case class WrappedParameterizedType(rawType: Type, ownerType: Type, typeArgs: Array[Type]) extends Type

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
    val alreadyConvertedBoundedTypes = new mutable.HashSet[BoundedType]

    def javaTypeAsScalaTypeIn(tpe: Type): String = tpe match {
      case clazz: Class[_] =>
        javaTypeAsScalaTypeIn(classToExistential(clazz))

      case TypeTag(underlyingType) =>
        javaTypeAsScalaTypeIn(underlyingType)

      case wildcardType: WildcardType =>
        javaTypeAsScalaTypeIn(WildcardBoundedType(wildcardType))

      case typeVariable: TypeVariable[_] =>
        javaTypeAsScalaTypeIn(TypeVariableBoundedType(typeVariable))

      case boundedType@BoundedType(name, _, _) if alreadyConvertedBoundedTypes.contains(boundedType) =>
        name

      case boundedType@BoundedType(name, upperBounds, lowerBounds) =>
        alreadyConvertedBoundedTypes += boundedType

        val upperBoundsRepr = upperBounds.filter(_ != classOf[Object]).map(javaTypeAsScalaTypeIn).mkString(" with ")
        val lowerBoundsRepr = lowerBounds.map(javaTypeAsScalaTypeIn).mkString(" with ")

        name + (if (upperBoundsRepr.nonEmpty) (" <: " + upperBoundsRepr) else "") +
          (if (lowerBoundsRepr.nonEmpty) (" >: " + lowerBoundsRepr) else "")

      case WrappedParameterizedType(rawType, ownerType, typeArguments) =>
        val clazz = rawType.asInstanceOf[Class[_]]
        val typeArgumentsRepr = if (typeArguments.nonEmpty) typeArguments.map(javaTypeAsScalaTypeIn).mkString("[", ", ", "]") else ""

        if (ownerType != null) {
          javaTypeAsScalaTypeIn(ownerType) + "#" + clazz.getSimpleName + typeArgumentsRepr
        } else {
          javaTypeAsScalaTypeIn(RawClass(clazz)) + typeArgumentsRepr
        }

      case paramType: ParameterizedType =>
        javaTypeAsScalaTypeIn(parameterizedTypeToExistential(paramType))

      case GenericArrayType(componentType) =>
        s"Array[${javaTypeAsScalaTypeIn(componentType)}]"

      case ExistentialType(polyType, typeVars) =>
        def typeVarDefs = typeVars.map(tv => s"type ${javaTypeAsScalaType(tv)}").mkString(" forSome {", "; ", "}")
        javaTypeAsScalaTypeIn(polyType) + (if (typeVars.nonEmpty) typeVarDefs else "")

      case RawClass(arrayClazz) if arrayClazz.isArray =>
        s"Array[${javaTypeAsScalaTypeIn(arrayClazz.getComponentType)}]"

      case RawClass(clazz) if clazz.isPrimitive =>
        primitiveTypes(clazz)

      case RawClass(clazz) if !clazz.isAnonymousClass && !clazz.isSynthetic =>
        clazz.getCanonicalName

      case TypeVariableRef(name) =>
        name

      case _ =>
        throw new IllegalArgumentException(s"Cannot convert $tpe into Scala type")
    }

    javaTypeAsScalaTypeIn(tpe)
  }

  def erasureOf(tpe: Type): Class[_] = tpe match {
    case clazz: Class[_] =>
      clazz
    case RawClass(clazz) =>
      clazz
    case TypeTag(underlyingType) =>
      erasureOf(underlyingType)
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
    def transformTypeArgs(typeArgs: Array[Type]): (Array[Type], List[BoundedType]) = {
      val typeVariablesBuffer = new ListBuffer[BoundedType]

      // little ugly: relying on side effects inside map
      val transformedArgs = typeArgs.map {
        case wc: WildcardType =>
          val name = newTypeVarName()
          typeVariablesBuffer += ScalaTypeVariable(name, wc.getUpperBounds, wc.getLowerBounds)
          TypeVariableRef(name)
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

    val typeVariables = ownerTypeVariables ::: clazz.getTypeParameters.map(TypeVariableBoundedType).toList

    val typeArgs: Array[Type] =
      clazz.getTypeParameters.map(typeParam => TypeVariableRef(typeParam.getName))

    ExistentialType(WrappedParameterizedType(clazz, ownerType, typeArgs), typeVariables)
  }

  /**
   * Example: returns string <tt>[T <: java.lang.Cloneable, _ >: String]</tt> when passed list of two BoundedTypes
   * representing type variable <tt>T extends java.lang.Cloneable</tt> and wildcard type <tt>? super String</tt>.
   *
   * @param typeVars
   * @return
   */
  def appliedBoundedTypes(typeVars: List[BoundedType]): String = {
    if (typeVars.nonEmpty)
      typeVars.map(javaTypeAsScalaType).mkString("[", ", ", "]")
    else
      ""
  }
}
