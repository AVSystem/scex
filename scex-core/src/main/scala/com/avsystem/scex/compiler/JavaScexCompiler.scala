package com.avsystem.scex.compiler

import com.avsystem.scex.compiler.ScexCompiler.CompilationFailedException
import com.avsystem.scex.{TypeTag, Expression}
import java.lang.reflect.Type
import java.{util => ju, lang => jl}
import JavaTypeParsing._

class JavaScexCompiler(config: ScexCompilerConfig) extends ScexCompiler(config) {

  @throws[CompilationFailedException]
  def getCompiledStringExpression[C](
    profile: ExpressionProfile,
    expression: String,
    contextClass: Class[C]): Expression[C, String] = {

    getCompiledStringExpression(profile, expression, contextClass: Type)
  }

  @throws[CompilationFailedException]
  def getCompiledStringExpression[C](
    profile: ExpressionProfile,
    expression: String,
    contextType: TypeTag[C]): Expression[C, String] = {

    getCompiledStringExpression(profile, expression, contextType: Type)
  }

  @throws[CompilationFailedException]
  private def getCompiledStringExpression[C](
    profile: ExpressionProfile,
    expression: String,
    contextType: Type): Expression[C, String] = {

    val scalaContextType = javaTypeAsScalaType(contextType)
    val contextClass = erasureOf(contextType)

    getCompiledStringExpression(profile, expression, scalaContextType, contextClass)
  }

  @throws[CompilationFailedException]
  def getCompiledExpression[C, R](
    profile: ExpressionProfile,
    expression: String,
    contextType: Class[C],
    resultType: Class[R]): Expression[C, R] = {

    getCompiledExpression(profile, expression, contextType: Type, resultType: Type)
  }

  @throws[CompilationFailedException]
  def getCompiledExpression[C, R](
    profile: ExpressionProfile,
    expression: String,
    contextType: Class[C],
    resultType: TypeTag[R]): Expression[C, R] = {

    getCompiledExpression(profile, expression, contextType: Type, resultType: Type)
  }

  @throws[CompilationFailedException]
  def getCompiledExpression[C, R](
    profile: ExpressionProfile,
    expression: String,
    contextType: TypeTag[C],
    resultType: Class[R]): Expression[C, R] = {

    getCompiledExpression(profile, expression, contextType: Type, resultType: Type)
  }

  @throws[CompilationFailedException]
  def getCompiledExpression[C, R](
    profile: ExpressionProfile,
    expression: String,
    contextType: TypeTag[C],
    resultType: TypeTag[R]): Expression[C, R] = {

    getCompiledExpression(profile, expression, contextType: Type, resultType: Type)
  }

  @throws[CompilationFailedException]
  private def getCompiledExpression[C, R](
    profile: ExpressionProfile,
    expression: String,
    contextType: Type,
    resultType: Type): Expression[C, R] = {

    val scalaContextType = javaTypeAsScalaType(contextType)
    val contextClass = erasureOf(contextType)
    val scalaResultType = javaTypeAsScalaType(resultType)

    getCompiledExpression[C, R](profile, expression, scalaContextType, contextClass, scalaResultType)
  }

}
