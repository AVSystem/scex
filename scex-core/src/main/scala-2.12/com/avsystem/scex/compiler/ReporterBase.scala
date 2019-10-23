package com.avsystem.scex.compiler

import scala.reflect.internal.util.Position
import scala.tools.nsc.reporters.AbstractReporter

abstract class ReporterBase extends AbstractReporter {
  def doReport(pos: Position, msg: String, severity: Severity): Unit

  final def display(pos: Position, msg: String, severity: Severity): Unit =
    doReport(pos, msg, severity)
}
