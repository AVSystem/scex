package com.avsystem.scex.compiler

import java.{util => ju, lang => jl}

/**
 *
 * Scex compiler that accepts modified, XML-friendly syntax:
 * <ul>
 * <li>string literals can be enclosed in both single and double quotes</li>
 * <li>identifiers 'lt', 'gt', 'lte', 'gte', 'and' and 'or' are
 * aliases of '<', '>', '<=', '>=', '&&' and '||'</li>
 * </ul>
 *
 * Created: 16-08-2013
 * Author: ghik
 */
trait XmlFriendlyScexCompiler extends ScexPresentationCompiler {
  override protected def compileExpression(exprDef: ExpressionDef) =
    super.compileExpression(exprDef.copy(expression = XmlFriendlyTranslator.translate(exprDef.expression).result))

  override protected def getInteractiveContext(
    profile: ExpressionProfile,
    contextType: String,
    rootObjectClass: Class[_],
    resultType: String) = {

    val wrapped: InteractiveContext =
      super.getInteractiveContext(profile, contextType, rootObjectClass, resultType)

    new InteractiveContext {
      def getErrors(expression: String) =
        wrapped.getErrors(XmlFriendlyTranslator.translate(expression).result)

      def getTypeCompletion(expression: String, position: Int) = {
        val ps = XmlFriendlyTranslator.translate(expression)
        wrapped.getTypeCompletion(ps.result, ps.positionMapping(position))
      }

      def getScopeCompletion(expression: String, position: Int) = {
        val ps = XmlFriendlyTranslator.translate(expression)
        wrapped.getScopeCompletion(ps.result, ps.positionMapping(position))
      }
    }
  }

}
