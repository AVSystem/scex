package com.avsystem.scex
package compiler

import java.util.concurrent.TimeUnit
import java.{lang => jl, util => ju}

import com.avsystem.scex.compiler.ScexCompiler.{CompilationFailedException, CompileError}
import com.avsystem.scex.compiler.TemplateOptimizingScexCompiler.ConversionSupplier
import com.avsystem.scex.compiler.presentation.ScexPresentationCompiler
import com.avsystem.scex.util.Literal
import com.google.common.cache.CacheBuilder
import org.apache.commons.codec.digest.DigestUtils

import scala.util.{Success, Try}

/**
 * Avoids actual compilation of most simple template literal expressions by trying to parse them
 * immediately into resulting values. This also means that conversion of literal values to expected result type
 * is performed immediately during compilation and conversion errors will be reported as compilation errors.
 *
 * Created: 01-04-2014
 * Author: ghik
 */
trait TemplateOptimizingScexCompiler extends ScexPresentationCompiler {

  import com.avsystem.scex.util.CacheImplicits._

  private val literalConversionsCache = CacheBuilder.newBuilder
    .expireAfterAccess(settings.expressionExpirationTime.value, TimeUnit.SECONDS)
    .build[(ExpressionProfile, String, String), Try[Literal => Any]]((compileLiteralConversion _).tupled)

  private def getLiteralConversion(exprDef: ExpressionDef) =
    literalConversionsCache.get((exprDef.profile, exprDef.resultType, exprDef.header))

  private case class LiteralExpression(value: Any)(val debugInfo: ExpressionDebugInfo) extends RawExpression {
    def apply(ctx: ExpressionContext[_, _]) = value
  }

  val interpolatedParamStart = "(^|[^$])(\\$\\$)*\\$([^$]|$)".r

  /**
   * Compiles a dummy expression that tests if there is a valid implicit conversion from Literal to expected type
   * that is not a macro and does not reference context or root object (and thus is independent of expression input).
   * If there is no such conversion, <code>LiteralsOptimizingScexCompiler</code> will not attempt to optimize the
   * compilation and simply pass it to <code>super.compileExpression</code>.
   */
  private def validateLiteralConversion(exprDef: ExpressionDef) = {
    import com.avsystem.scex.compiler.CodeGeneration._
    val actualHeader = implicitLiteralViewHeader(exprDef.header)
    val validationExpression = implicitLiteralViewExpression(exprDef.resultType)
    val validationExprDef = ExpressionDef(exprDef.profile, template = false, setter = false,
      validationExpression, validationExpression, PositionMapping.empty, actualHeader, exprDef.rootObjectClass,
      exprDef.contextType, exprDef.resultType)
    super.compileExpression(validationExprDef)
  }

  private def compileLiteralConversion(profile: ExpressionProfile, resultType: String, header: String) = underLock {
    import com.avsystem.scex.compiler.CodeGeneration._

    val profileObjectPkg = compileProfileObject(profile).get
    val utilsObjectPkg = compileExpressionUtils(profile.expressionUtils).get
    val conversionClassCode = implicitLiteralConversionClass(profileObjectPkg, utilsObjectPkg, profile.expressionHeader, header, resultType)
    val pkgName = ConversionSupplierPkgPrefix + DigestUtils.md5Hex(conversionClassCode)
    val fullCode = wrapInSource(conversionClassCode, pkgName)

    def result =
      compile(new ScexSourceFile(pkgName, fullCode, shared = false)) match {
        case Left(classLoader) =>
          Class.forName(s"$pkgName.$ConversionSupplierClassName", true, classLoader)
            .newInstance.asInstanceOf[ConversionSupplier[Any]].get
        case Right(errors) =>
          throw new CompilationFailedException(fullCode, errors)
      }

    Try(result)
  }

  private def toCompileError(expr: String, throwable: Throwable) =
    new CompileError(expr, 1, throwable.getClass.getName + ": " + throwable.getMessage)

  private def isEligible(exprDef: ExpressionDef) =
    exprDef.template && !exprDef.setter &&
      !interpolatedParamStart.findFirstIn(exprDef.expression).isDefined &&
      (isStringSupertype(exprDef.resultType) || validateLiteralConversion(exprDef).isSuccess)

  private def toLiteral(exprDef: ExpressionDef) =
    Literal(exprDef.expression.replaceAllLiterally("$$", "$"))

  private def isStringSupertype(tpe: String) =
    JavaTypeParsing.StringSupertypes.contains(tpe)

  override protected def compileExpression(exprDef: ExpressionDef) = {
    lazy val sourceInfo = new SourceInfo(null, exprDef.expression, 0, exprDef.expression.length, 1, exprDef.expression.count(_ == '\n') + 2)
    lazy val debugInfo = new ExpressionDebugInfo(exprDef, sourceInfo)

    if (isEligible(exprDef)) {
      val literal = toLiteral(exprDef)
      if (isStringSupertype(exprDef.resultType))
        Success(LiteralExpression(literal.literalString)(debugInfo))
      else getLiteralConversion(exprDef).map { conversion =>
        try LiteralExpression(conversion(literal))(debugInfo) catch {
          case throwable: Throwable =>
            throw new CompilationFailedException(literal.literalString,
              List(toCompileError(literal.literalString, throwable)))
        }
      }
    } else super.compileExpression(exprDef)
  }

  override protected def getErrors(exprDef: ExpressionDef) = super.getErrors(exprDef) match {
    case Nil if isEligible(exprDef) && !isStringSupertype(exprDef.resultType) =>
      getLiteralConversion(exprDef).map { conversion =>
        val literal = toLiteral(exprDef)
        try {
          conversion(literal)
          Nil
        } catch {
          case throwable: Throwable =>
            List(toCompileError(literal.literalString, throwable))
        }
      }.getOrElse(Nil)

    case errors => errors
  }

  override def reset() = underLock {
    super.reset()
    literalConversionsCache.invalidateAll()
  }
}

object TemplateOptimizingScexCompiler {

  trait ConversionSupplier[+T] {
    def get: Literal => T
  }

  import scala.language.experimental.macros

  def reifyImplicitView[T](arg: Any): T = macro Macros.reifyImplicitView_impl[T]

  def checkConstant[T](expr: T): T = macro Macros.checkConstantExpr_impl[T]
}