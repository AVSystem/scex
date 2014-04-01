package com.avsystem.scex
package compiler

import java.{util => ju, lang => jl}
import com.avsystem.scex.util.MacroUtils
import scala.tools.nsc.Global
import java.security.MessageDigest
import scala.io.Codec

/**
 * Code copied from scala.reflect.runtime.JavaMirrors and scala.reflect.internal.StdNames
 * to get Java classes for Scala types.
 *
 * Created: 01-04-2014
 * Author: ghik
 */
abstract class GlobalUtils protected extends MacroUtils {
  val universe: Global

  import universe._
  import universe.definitions._

  private val PackageAndClassPattern = """(.*\.)(.*)$""".r

  private def jArrayClass(elemClazz: Class[_]): Class[_] = {
    jl.reflect.Array.newInstance(elemClazz, 0).getClass
  }

  def erasureClass(tpe: Type) = try typeToJavaClass(tpe.erasure) catch {
    case _: ClassNotFoundException | _: NoClassDefFoundError => null
  }

  /** The Java class that corresponds to given Scala type.
    * Pre: Scala type is already transformed to Java level.
    */
  def typeToJavaClass(tpe: Type): Class[_] = tpe match {
    case ExistentialType(_, rtpe) => typeToJavaClass(rtpe)
    case TypeRef(_, ArrayClass, List(elemtpe)) => jArrayClass(typeToJavaClass(elemtpe))
    case TypeRef(_, sym: ClassSymbol, _) => classToJava(sym.asClass)
    case tpe@TypeRef(_, sym: AliasTypeSymbol, _) => typeToJavaClass(tpe.dealias)
    case SingleType(_, sym: ModuleSymbol) => classToJava(sym.moduleClass.asClass)
    case _ => throw new NoClassDefFoundError("no Java class corresponding to " + tpe + " found")
  }

  /** The Java class corresponding to given Scala class.
    * Note: This only works for
    * - top-level classes
    * - Scala classes that were generated via jclassToScala
    * - classes that have a class owner that has a corresponding Java class
    * @throws A `ClassNotFoundException` for all Scala classes not in one of these categories.
    */
  @throws(classOf[ClassNotFoundException])
  private def classToJava(clazz: ClassSymbol): Class[_] = {
    def noClass = throw new ClassNotFoundException("no Java class corresponding to " + clazz + " found")
    //println("classToJava "+clazz+" "+clazz.owner+" "+clazz.owner.isPackageClass)//debug
    if (clazz.isPrimitiveValueClass)
      valueClassToJavaType(clazz)
    else if (clazz == ArrayClass)
      noClass
    else if (clazz.owner.isPackageClass)
      javaClass(clazz.javaClassName)
    else if (clazz.owner.isClass) {
      val childOfClass = !clazz.owner.isModuleClass
      val childOfTopLevel = clazz.owner.owner.isPackageClass
      val childOfTopLevelObject = clazz.owner.isModuleClass && childOfTopLevel

      // suggested in https://issues.scala-lang.org/browse/SI-4023?focusedCommentId=54759#comment-54759
      var ownerClazz = classToJava(clazz.owner.asClass)
      if (childOfTopLevelObject) ownerClazz = Class.forName(ownerClazz.getName stripSuffix "$", true, ownerClazz.getClassLoader)
      val ownerChildren = ownerClazz.getDeclaredClasses

      var fullNameOfJavaClass = ownerClazz.getName
      if (childOfClass || childOfTopLevel) fullNameOfJavaClass += "$"
      fullNameOfJavaClass += clazz.name

      // compactify (see SI-7779)
      fullNameOfJavaClass = fullNameOfJavaClass match {
        case PackageAndClassPattern(pack, clazzName) =>
          // in a package
          pack + compactifyName(clazzName)
        case _ =>
          // in the empty package
          compactifyName(fullNameOfJavaClass)
      }

      if (clazz.isModuleClass) fullNameOfJavaClass += "$"

      ownerChildren.find(_.getName == fullNameOfJavaClass).getOrElse(noClass)
    } else
      noClass
  }

  private def javaClass(path: String): Class[_] =
    Class.forName(path)

  private def compactifyName(orig: String): String = compactify(orig)

  private object compactify extends (String => String) {
    val md5 = MessageDigest.getInstance("MD5")

    /**
     * COMPACTIFY
     *
     * The hashed name has the form (prefix + marker + md5 + marker + suffix), where
     * - prefix/suffix.length = MaxNameLength / 4
     * - md5.length = 32
     *
     * We obtain the formula:
     *
     * FileNameLength = 2*(MaxNameLength / 4) + 2.marker.length + 32 + 6
     *
     * (+6 for ".class"). MaxNameLength can therefore be computed as follows:
     */
    val marker = "$$$$"
    val MaxNameLength = math.min(
      settings.maxClassfileName.value - 6,
      2 * (settings.maxClassfileName.value - 6 - 2 * marker.length - 32)
    )

    def toMD5(s: String, edge: Int): String = {
      val prefix = s take edge
      val suffix = s takeRight edge

      val cs = s.toArray
      val bytes = Codec toUTF8 cs
      md5 update bytes
      val md5chars = (md5.digest() map (b => (b & 0xFF).toHexString)).mkString

      prefix + marker + md5chars + marker + suffix
    }

    def apply(s: String): String =
      if (s.length <= MaxNameLength) s
      else toMD5(s, MaxNameLength / 4)
  }

}

object GlobalUtils {
  def apply(g: Global) = new GlobalUtils {
    val universe: g.type = g
  }
}
