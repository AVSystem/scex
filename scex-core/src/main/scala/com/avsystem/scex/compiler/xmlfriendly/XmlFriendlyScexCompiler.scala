package com.avsystem.scex
package compiler.xmlfriendly

import com.avsystem.scex.compiler.{ScexCompiler, ExpressionDef}
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
trait XmlFriendlyScexCompiler extends ScexCompiler {
  override def preprocess(exprDef: ExpressionDef) = {
    val ps = XmlFriendlyTranslator.translate(exprDef.expression, exprDef.template)
    exprDef.copy(expression = ps.result, positionMapping = ps.positionMapping)
  }
}
