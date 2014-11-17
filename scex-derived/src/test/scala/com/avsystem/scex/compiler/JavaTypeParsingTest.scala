package com.avsystem.scex
package compiler

import java.{lang => jl, util => ju}

class JavaTypeParsingTest extends ScexFunSuite {

  import com.avsystem.scex.compiler.JavaTypeParsing._

  test("toplevel non-parameterized classes") {
    assertResult("java.lang.Object") {
      javaTypeAsScalaType(classOf[Object])
    }
    assertResult("com.avsystem.scex.compiler.OuterClass") {
      javaTypeAsScalaType(classOf[OuterClass])
    }
  }

  test("static inner classes") {
    assertResult("com.avsystem.scex.compiler.OuterClass.InnerStaticClass") {
      javaTypeAsScalaType(classOf[OuterClass.InnerStaticClass])
    }
  }

  test("non-static inner classes") {
    assertResult("com.avsystem.scex.compiler.OuterClass#InnerClass") {
      javaTypeAsScalaType(classOf[OuterClass#InnerClass])
    }
  }

  test("raw parameterized classes") {
    assertResult("com.avsystem.scex.compiler.ParameterizedClass[T] forSome {type T}") {
      javaTypeAsScalaType(classOf[ParameterizedClass[_]])
    }
  }

  test("raw parameterized classes with bounds") {
    assertResult("com.avsystem.scex.compiler.BoundedParameterizedClass[T] forSome {type T <: java.io.InputStream with java.io.Closeable}") {
      javaTypeAsScalaType(classOf[BoundedParameterizedClass[_]])
    }
  }

  test("recursively generic classes") {
    assertResult("com.avsystem.scex.compiler.RecursiveGenericClass[T] forSome {type T <: java.lang.Comparable[T]}") {
      javaTypeAsScalaType(classOf[RecursiveGenericClass[_]])
    }
  }

  test("deeply nested parameterized classes") {
    assertResult("com.avsystem.scex.compiler.ParameterizedClass.StaticInnerGeneric[A]#DeeplyInnerGeneric[B] forSome {type A <: java.lang.Cloneable; type B}") {
      javaTypeAsScalaType(classOf[ParameterizedClass.StaticInnerGeneric[A]#DeeplyInnerGeneric[B] forSome {type A; type B}])
    }
  }

  test("simple parameterized types") {
    assertResult("java.lang.Comparable[java.lang.String]") {
      javaTypeAsScalaType(JavaTypes.comparableOfString())
    }
  }

  test("simple wildcard types") {
    assertResult("java.lang.Comparable[T1] forSome {type T1}") {
      javaTypeAsScalaType(JavaTypes.comparableOfWildcard())
    }
  }

  test("deeply nested parameterized types") {
    assertResult("com.avsystem.scex.compiler.ParameterizedClass.StaticInnerGeneric[T1]#DeeplyInnerGeneric[T2] forSome {type T1; type T2}") {
      javaTypeAsScalaType(JavaTypes.complexParameterizedType())
    }
  }

  test("deeply nested partially wildcarded parameterized types") {
    assertResult("com.avsystem.scex.compiler.ParameterizedClass.StaticInnerGeneric[java.lang.Cloneable]#DeeplyInnerGeneric[T1] forSome {type T1}") {
      javaTypeAsScalaType(JavaTypes.partiallyWildcardedParameterizedType())
    }
  }
}