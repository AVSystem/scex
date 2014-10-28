package com.avsystem.scex
package compiler

import java.{util => ju, lang => jl}
import scala.tools.nsc.Global
import java.security.MessageDigest
import scala.io.Codec
import scala.reflect.internal.util._
import com.avsystem.scex.util.MacroUtils
import com.avsystem.scex.compiler.ScexCompiler.CompileError
import scala.collection.mutable.ListBuffer
import scala.tools.nsc.plugins.Plugin

/**
 * Created: 01-04-2014
 * Author: ghik
 */
trait ScexGlobal extends Global with MacroUtils with JavaClassComputation {
  val universe: this.type = this

  def loadAdditionalPlugins(): List[Plugin] = Nil

  class ParsingCompilationUnit(source: SourceFile) extends CompilationUnit(source) {
    private val errorsBuilder = new ListBuffer[CompileError]

    def errors =
      errorsBuilder.result()

    override def echo(pos: Position, msg: String) = ()

    override def error(pos: Position, msg: String) =
      errorsBuilder += CompileError(pos.lineContent, if (pos.isDefined) pos.column else 1, msg)

    override def warning(pos: Position, msg: String) = ()

    override def deprecationWarning(pos: Position, msg: String) = ()

    override def uncheckedWarning(pos: Position, msg: String) = ()

    override def inlinerWarning(pos: Position, msg: String) = ()

    override def incompleteInputError(pos: Position, msg: String) = ()

    override def comment(pos: Position, msg: String) = ()
  }

  def parseExpression(code: String, template: Boolean) = {
    val (wrappedCode, offset) = CodeGeneration.wrapForParsing(code, template)
    val sourceFile = new BatchSourceFile("(for_parsing)", wrappedCode)
    val unit = new ParsingCompilationUnit(sourceFile)
    val PackageDef(_, List(ModuleDef(_, _, Template(_, _, List(_, expressionTree))))) = new syntaxAnalyzer.UnitParser(unit).parse()
    (moveTree(expressionTree, -offset), unit.errors)
  }

  def movePosition(pos: Position, offset: Int) = pos match {
    case tp: TransparentPosition => new TransparentPosition(tp.source, tp.start + offset, tp.point + offset, tp.end + offset)
    case rp: RangePosition => new RangePosition(rp.source, rp.start + offset, rp.point + offset, rp.end + offset)
    case op: OffsetPosition => new OffsetPosition(op.source, op.point + offset)
    case _ => pos
  }

  def moveTree(tree: Tree, offset: Int) = {
    tree.foreach { t =>
      t.setPos(movePosition(t.pos, offset))
    }
    tree
  }

  /**
   * Locator with slightly modified inclusion check.
   *
   * @param pos
   */
  class Locator(pos: Position) extends Traverser {
    var last: Tree = _

    def locateIn(root: Tree): Tree = {
      this.last = EmptyTree
      traverse(root)
      this.last
    }

    override def traverse(t: Tree) {
      t match {
        case tt: TypeTree if tt.original != null && includes(tt.pos, tt.original.pos) =>
          traverse(tt.original)
        case _ =>
          if (includes(t.pos, pos)) {
            if (!t.pos.isTransparent) last = t
            super.traverse(t)
          } else t match {
            case mdef: MemberDef =>
              traverseTrees(mdef.mods.annotations)
            case _ =>
          }
      }
    }

    private def includes(pos1: Position, pos2: Position) =
      (pos1 includes pos2) && pos1.end > pos2.start
  }

  override protected def loadRoughPluginsList() =
    loadAdditionalPlugins() ::: super.loadRoughPluginsList()
}
