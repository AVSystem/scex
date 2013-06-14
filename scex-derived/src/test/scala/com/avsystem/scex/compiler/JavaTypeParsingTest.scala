package com.avsystem.scex.compiler

import java.{util => ju, lang => jl}
import org.scalatest._


class JavaTypeParsingTest extends FunSuite {

  import JavaTypeParsing._

  test("toplevel non-parameterized classes") {
    expectResult("java.lang.Object") {
      javaTypeAsScalaType(classOf[Object])
    }
    expectResult("com.avsystem.scex.compiler.OuterClass") {
      javaTypeAsScalaType(classOf[OuterClass])
    }
  }

  test("static inner classes") {
    expectResult("com.avsystem.scex.compiler.OuterClass.InnerStaticClass") {
      javaTypeAsScalaType(classOf[OuterClass.InnerStaticClass])
    }
  }

  test("non-static inner classes") {
    expectResult("com.avsystem.scex.compiler.OuterClass#InnerClass") {
      javaTypeAsScalaType(classOf[OuterClass#InnerClass])
    }
  }

  test("raw parameterized classes") {
    expectResult("com.avsystem.scex.compiler.ParameterizedClass[T] forSome {type T}") {
      javaTypeAsScalaType(classOf[ParameterizedClass[_]])
    }
  }

  test("raw parameterized classes with bounds") {
    expectResult("com.avsystem.scex.compiler.BoundedParameterizedClass[T] forSome {type T <: java.io.InputStream with java.io.Closeable}") {
      javaTypeAsScalaType(classOf[BoundedParameterizedClass[_]])
    }
  }

  test("recursively generic classes") {
    expectResult("com.avsystem.scex.compiler.RecursiveGenericClass[T] forSome {type T <: java.lang.Comparable[T]}") {
      javaTypeAsScalaType(classOf[RecursiveGenericClass[_]])
    }
  }

  test("deeply nested parameterized classes") {
    expectResult("com.avsystem.scex.compiler.ParameterizedClass.StaticInnerGeneric[A]#DeeplyInnerGeneric[B] forSome {type A <: java.lang.Cloneable; type B}") {
      javaTypeAsScalaType(classOf[ParameterizedClass.StaticInnerGeneric[A]#DeeplyInnerGeneric[B] forSome {type A; type B}])
    }
  }

  test("simple parameterized types") {
    expectResult("java.lang.Comparable[java.lang.String]") {
      javaTypeAsScalaType(JavaTypes.comparableOfString())
    }
  }

  test("simple wildcard types") {
    expectResult("java.lang.Comparable[T1] forSome {type T1}") {
      javaTypeAsScalaType(JavaTypes.comparableOfWildcard())
    }
  }

  test("deeply nested parameterized types") {
    expectResult("com.avsystem.scex.compiler.ParameterizedClass.StaticInnerGeneric[T1]#DeeplyInnerGeneric[T2] forSome {type T1; type T2}") {
      javaTypeAsScalaType(JavaTypes.complexParameterizedType())
    }
  }

  test("deeply nested partially wildcarded parameterized types") {
    expectResult("com.avsystem.scex.compiler.ParameterizedClass.StaticInnerGeneric[java.lang.Cloneable]#DeeplyInnerGeneric[T1] forSome {type T1}") {
      javaTypeAsScalaType(JavaTypes.partiallyWildcardedParameterizedType())
    }
  }
}