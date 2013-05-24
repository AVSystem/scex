package com.avsystem.scex.compiler

import java.{util => ju, lang => jl}
import org.scalatest._


class JavaTypeParsingTest extends FlatSpec {

  import JavaTypeParsing._

  "Class translation" should "return FQCN for toplevel non-parameterized classes" in {
    expectResult("java.lang.Object") {
      javaTypeAsScalaType(classOf[Object])
    }
    expectResult("com.avsystem.scex.compiler.OuterClass") {
      javaTypeAsScalaType(classOf[OuterClass])
    }
  }

  it should "return FQCN for static inner classes" in {
    expectResult("com.avsystem.scex.compiler.OuterClass.InnerStaticClass") {
      javaTypeAsScalaType(classOf[OuterClass.InnerStaticClass])
    }
  }

  it should "return proper type projection for non-static inner classes" in {
    expectResult("com.avsystem.scex.compiler.OuterClass#InnerClass") {
      javaTypeAsScalaType(classOf[OuterClass#InnerClass])
    }
  }

  it should "properly convert raw parameterized classes to existential types" in {
    expectResult("com.avsystem.scex.compiler.ParameterizedClass[T] forSome {type T}") {
      javaTypeAsScalaType(classOf[ParameterizedClass[_]])
    }
  }

  it should "properly convert raw parameterized classes with bounds to existential types" in {
    expectResult("com.avsystem.scex.compiler.BoundedParameterizedClass[T] forSome {type T <: java.io.InputStream with java.io.Closeable}") {
      javaTypeAsScalaType(classOf[BoundedParameterizedClass[_]])
    }
  }

  it should "properly handle recursive generic classes" in {
    expectResult("com.avsystem.scex.compiler.RecursiveGenericClass[T] forSome {type T <: java.lang.Comparable[T]}") {
      javaTypeAsScalaType(classOf[RecursiveGenericClass[_]])
    }
  }

  it should "properly handle deeply nested parameterized classes" in {
    expectResult("com.avsystem.scex.compiler.ParameterizedClass.StaticInnerGeneric[A]#DeeplyInnerGeneric[B] forSome {type A <: java.lang.Cloneable; type B}") {
      javaTypeAsScalaType(classOf[ParameterizedClass.StaticInnerGeneric[A]#DeeplyInnerGeneric[B] forSome {type A; type B}])
    }
  }

  "Parameterized type translation" should "properly convert simple parameterized type" in {
    expectResult("java.lang.Comparable[java.lang.String]") {
      javaTypeAsScalaType(JavaTypes.comparableOfString())
    }
  }

  it should "properly convert simple wildcard type" in {
    expectResult("java.lang.Comparable[T1] forSome {type T1}") {
      javaTypeAsScalaType(JavaTypes.comparableOfWildcard())
    }
  }

  it should "properly convert deeply nested parameterized types" in {
    expectResult("com.avsystem.scex.compiler.ParameterizedClass.StaticInnerGeneric[T1]#DeeplyInnerGeneric[T2] forSome {type T1; type T2}") {
      javaTypeAsScalaType(JavaTypes.complexParameterizedType())
    }
  }

  it should "properly convert deeply nested partially wildcarded parameterized types" in {
    expectResult("com.avsystem.scex.compiler.ParameterizedClass.StaticInnerGeneric[java.lang.Cloneable]#DeeplyInnerGeneric[T1] forSome {type T1}") {
      javaTypeAsScalaType(JavaTypes.partiallyWildcardedParameterizedType())
    }
  }
}