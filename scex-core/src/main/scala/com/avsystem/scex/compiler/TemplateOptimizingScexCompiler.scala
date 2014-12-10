package com.avsystem.scex
package compiler

import java.util.concurrent.TimeUnit
import java.{lang => jl, util => ju}

import com.avsystem.scex.compiler.ScexCompiler.{CompilationFailedException, CompileError}
import com.avsystem.scex.compiler.TemplateOptimizingScexCompiler.ConversionSupplier
import com.avsystem.scex.compiler.presentation.ScexPresentationCompiler
import com.avsystem.scex.parsing._
import com.avsystem.scex.util.Literal
import com.google.common.cache.CacheBuilder
import org.apache.commons.codec.digest.DigestUtils

import scala.annotation.tailrec
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/**
 * An extension of ScexCompiler which:
 *
 * <ul>
 * <li>Avoids actual compilation of most simple template literal expressions by trying to parse them
 * immediately into resulting values. This also means that conversion of literal values to expected result type
 * is performed immediately during compilation and conversion errors will be reported as compilation errors.</li>
 * <li>Manually parses template expressions and compiles each template argument as a separate expression.</li>
 * </ul>
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

  private class OptimizedTemplateExpression(parts: List[String], args: List[RawExpression], val debugInfo: ExpressionDebugInfo)
    extends RawExpression {

    def apply(c: ExpressionContext[_, _]) =
      TemplateInterpolations.concatIterator(parts: _*)(args.iterator.map { arg =>
        val value = arg.apply(c)
        if (value != null)
          value.toString
        else {
          val errorMsg = s"Template argument ${arg.debugInfo.definition.originalExpression} evaluated to null"
          throw new EvaluationException(None, new NullPointerException(errorMsg))
        }
      })
  }

  import com.avsystem.scex.parsing.TemplateParser.{Success => ParsingSuccess, parseTemplate}

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
    val validationExprDef = ExpressionDef(exprDef.profile, template = false, setter = false, validationExpression,
      actualHeader, exprDef.contextType, exprDef.resultType)(validationExpression, EmptyPositionMapping, exprDef.rootObjectClass)
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
          val clazz = Class.forName(s"$pkgName.$ConversionSupplierClassName", true, classLoader)
          clazz.getDeclaredClasses // force loading of inner classes
          clazz.newInstance.asInstanceOf[ConversionSupplier[Any]].get
        case Right(errors) =>
          throw new CompilationFailedException(fullCode, errors)
      }

    Try(result)
  }

  private def toCompileError(expr: String, resultType: String, throwable: Throwable) =
    new CompileError(expr, 1, s"Invalid literal value for $resultType: ${throwable.getClass.getName}: ${throwable.getMessage}")

  private def isStringSupertype(tpe: String) =
    JavaTypeParsing.StringSupertypes.contains(tpe)

  override protected def compileExpression(exprDef: ExpressionDef) = if (exprDef.template && !exprDef.setter) {
    lazy val debugInfo = new ExpressionDebugInfo(exprDef)

    parseTemplate(exprDef.expression) match {
      case ParsingSuccess((List(singlePart), Nil), _) =>

        if (isStringSupertype(exprDef.resultType))
          Success(LiteralExpression(singlePart)(debugInfo))
        else if (validateLiteralConversion(exprDef).isSuccess)
          getLiteralConversion(exprDef).map { conversion =>
            try LiteralExpression(conversion(Literal(singlePart)))(debugInfo) catch {
              case NonFatal(throwable) =>
                throw new CompilationFailedException(singlePart, List(toCompileError(singlePart, exprDef.resultType, throwable)))
            }
          }
        else super.compileExpression(exprDef)

      case ParsingSuccess((List("", ""), List(_)), _) =>
        super.compileExpression(exprDef)

      case ParsingSuccess((parts, args), _) if isStringSupertype(exprDef.resultType) =>
        val argExprTries = args.map { arg =>
          val shift = new SingleShiftPositionMapping(arg.beg)
          val reverseMapping = exprDef.positionMapping.reverse
          val originalArg = exprDef.originalExpression.substring(reverseMapping(arg.beg), reverseMapping(arg.end - 1) + 1)
          val shiftedMapping = shift andThen exprDef.positionMapping andThen shift.reverse

          compileExpression(
            ExpressionDef(exprDef.profile, template = true, setter = false, arg.result, exprDef.header,
              exprDef.contextType, "String")(originalArg, shiftedMapping, exprDef.rootObjectClass))
        }

        @tailrec
        def merge(exprs: List[Try[RawExpression]], successAcc: List[RawExpression], errorsAcc: List[CompileError]): Try[List[RawExpression]] =
          exprs match {
            case Success(expr) :: rest =>
              merge(rest, expr :: successAcc, errorsAcc)
            case Failure(CompilationFailedException(_, errors)) :: rest =>
              merge(rest, successAcc, errors ::: errorsAcc)
            case Failure(throwable) :: _ =>
              Failure(throwable)
            case Nil =>
              if (errorsAcc.nonEmpty)
                Failure(CompilationFailedException(exprDef.expression, errorsAcc))
              else
                Success(successAcc)
          }

        merge(argExprTries.reverse, Nil, Nil).map(new OptimizedTemplateExpression(parts, _, debugInfo))

      case _ =>
        super.compileExpression(exprDef)
    }
  } else super.compileExpression(exprDef)

  override protected def getErrors(exprDef: ExpressionDef) = super.getErrors(exprDef) match {
    case Nil if exprDef.template && !exprDef.setter && !isStringSupertype(exprDef.resultType) =>
      parseTemplate(exprDef.expression) match {
        case ParsingSuccess((List(singlePart), Nil), _) if validateLiteralConversion(exprDef).isSuccess =>
          getLiteralConversion(exprDef).map { conversion =>
            val literal = Literal(singlePart)
            try {
              conversion(literal)
              Nil
            } catch {
              case throwable: Throwable =>
                List(toCompileError(singlePart, exprDef.resultType, throwable))
            }
          }.getOrElse(Nil)

        case _ => Nil
      }
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