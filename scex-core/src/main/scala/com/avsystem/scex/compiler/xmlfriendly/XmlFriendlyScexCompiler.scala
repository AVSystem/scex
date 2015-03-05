package com.avsystem.scex
package compiler.xmlfriendly

import java.{lang => jl, util => ju}

import com.avsystem.scex.compiler.ScexCompiler

/**
 *
 * Scex compiler that accepts modified, XML-friendly syntax:
 * <ul>
 * <li>string literals can be enclosed in both single and double quotes</li>
 * <li>identifiers 'lt', 'gt', 'lte', 'gte', 'and' and 'or' are
 * aliases of '<', '>', '<=', '>=', '&&' and '||'</li>
 * <li>interpolation arguments must always be blocks, i.e <tt>${ident}</tt>, not <tt>$ident</tt>
 * <li>dollars need not be escaped using two dollars</li>
 * </ul>
 *
 * Created: 16-08-2013
 * Author: ghik
 */
trait XmlFriendlyScexCompiler extends ScexCompiler {
  override protected def preprocess(expression: String, template: Boolean) = {
    val (superExpression, superMapping) = super.preprocess(expression, template)
    val ps = XmlFriendlyTranslator.translate(superExpression, template)
    (ps.result, ps.positionMapping andThen superMapping)
  }
}
