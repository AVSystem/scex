package com.avsystem.scex
package compiler.presentation.ast

import scala.collection.JavaConverters._
import com.avsystem.scex.util.CommonUtils

sealed trait Tree extends PrettyPrint {

  import CommonUtils._

  def attachments: Attachments

  lazy val children = {
    def childrenIn(obj: Any): List[Tree] = obj match {
      case tree: Tree => List(tree)
      case list: List[_] => list.flatMap(childrenIn)
      case _ => Nil
    }
    productIterator.flatMap(childrenIn).toList
  }

  def locate(pos: Int): Tree =
    locate(Position(pos, pos, transparent = false))

  def locate(start: Int, end: Int): Tree =
    locate(Position(start, end, transparent = false))

  def locate(pos: Position): Tree = {
    if (pos != null) {
      var result: Tree = EmptyTree
      def traverse(tree: Tree) {
        tree match {
          case EmptyTree =>
          case tt: TypeTree if tt.original != null && tt.original.attachments.position.includes(pos) =>
            traverse(tt.original)
          case _ if tree.attachments != null && tree.attachments.position != null && tree.attachments.position.includes(pos) =>
            if (!tree.attachments.position.transparent) {
              result = tree
            }
            tree.children.foreach(traverse)
          case md: MemberDef =>
            md.mods.annotations.foreach(traverse)
          case _ =>
        }
      }
      traverse(this)
      result
    } else EmptyTree
  }

  def pretty(withPositions: Boolean, withTypes: Boolean) = {
    val sb = new StringBuilder
    def pretty(indent: Int, obj: Any) {
      val newline = "\n" + "  " * indent
      def prettyElements(prefix: String, elems: Vector[Any]) {
        sb.append(prefix + "(")
        elems.size match {
          case 0 =>
          case 1 =>
            pretty(indent, elems.head)
          case _ =>
            sb.append(newline).append("  ")
            elems.init.foreach { e =>
              pretty(indent + 1, e)
              sb.append(",").append(newline).append("  ")
            }
            pretty(indent + 1, elems.last)
            sb.append(newline)
        }
        sb.append(")")
      }
      val treeOpt = obj match {
        case tree: Tree => Some(tree)
        case _ => None
      }
      if (withPositions) {
        treeOpt.foreach { tree =>
          sb.append(tree.attachments.position.toOpt.getOrElse("<no position>")).append(" ")
        }
      }
      obj match {
        case EmptyTree =>
          sb.append(EmptyTree)
        case product: PrettyPrint =>
          prettyElements(product.productPrefix, product.productIterator.toVector)
        case list: List[Any] =>
          prettyElements("List", list.toVector)
        case _ =>
          sb.append(obj)
      }
      if (withTypes) {
        treeOpt.foreach { tree =>
          sb.append(": ").append(tree.attachments.tpe.toOpt.getOrElse("<no type>"))
        }
      }
    }
    pretty(0, this)
    sb.result()
  }

  def childrenAsJava = children.asJava
}

abstract class AbstractTree(val attachments: Attachments) extends Tree

/**
 * A Scex-specific tree which is just a nicer representation of template expression.
 * This tree does not exist in the Scala compiler API.
 */
case class TemplateInterpolation(parts: List[Literal], args: List[Tree])(attachments: Attachments)
  extends AbstractTree(attachments) with TermTree {

  def partsAsJava = parts.asJava

  def argsAsJava = args.asJava
}

case class Annotated(annot: Tree, arg: Tree)(attachments: Attachments)
  extends AbstractTree(attachments)

case class CaseDef(pat: Tree, guard: Tree, body: Tree)(attachments: Attachments)
  extends AbstractTree(attachments)

sealed trait DefTree extends NameTree

case class Bind(name: Name, body: Tree)(attachments: Attachments)
  extends AbstractTree(attachments) with DefTree

case class LabelDef(name: TermName, params: List[Ident], rhs: Tree)(attachments: Attachments)
  extends AbstractTree(attachments) with DefTree with TermTree {

  def paramsAsJava = params.asJava
}

sealed trait MemberDef extends DefTree {
  def mods: Modifiers
}

sealed trait ImplDef extends MemberDef {
  def impl: Template
}

case class ClassDef(mods: Modifiers, name: TypeName, tparams: List[TypeDef], impl: Template)(attachments: Attachments)
  extends AbstractTree(attachments) with ImplDef {

  def tparamsAsJava = tparams.asJava
}

case class ModuleDef(mods: Modifiers, name: TermName, impl: Template)(attachments: Attachments)
  extends AbstractTree(attachments) with ImplDef

case class PackageDef(pid: RefTree, stats: List[Tree])(attachments: Attachments)
  extends AbstractTree(attachments) with MemberDef {

  def name = pid.name

  def mods = Modifiers.empty

  def statsAsJava = stats.asJava
}

case class TypeDef(mods: Modifiers, name: TypeName, tparams: List[TypeDef], rhs: Tree)(attachments: Attachments)
  extends AbstractTree(attachments) with MemberDef {

  def tparamsAsJava = tparams.asJava
}

sealed trait ValOrDefDef extends MemberDef {
  def tpt: Tree

  def rhs: Tree
}

case class DefDef(mods: Modifiers, name: Name, tparams: List[TypeDef], vparamss: List[List[ValDef]],
  tpt: Tree, rhs: Tree)(attachments: Attachments) extends AbstractTree(attachments) with ValOrDefDef {

  def tparamsAsJava = tparams.asJava

  def vparamssAsJava = vparamss.map(_.asJava).asJava
}

case class ValDef(mods: Modifiers, name: TermName, tpt: Tree, rhs: Tree)(attachments: Attachments)
  extends AbstractTree(attachments) with ValOrDefDef

case class Function(vparams: List[ValDef], body: Tree)(attachments: Attachments)
  extends AbstractTree(attachments) with TermTree {

  def vparamsAsJava = vparams.asJava
}

case class ImportSelector(name: Name, namePos: Int, rename: Name, renamePos: Int)

case class Import(expr: Tree, selectors: List[ImportSelector])(attachments: Attachments)
  extends AbstractTree(attachments) {

  def selectorsAsJava = selectors.asJava
}

case class Return(expr: Tree)(attachments: Attachments)
  extends AbstractTree(attachments) with TermTree

case class Template(parents: List[Tree], self: ValDef, body: List[Tree])(attachments: Attachments)
  extends AbstractTree(attachments) {

  def parentsAsJava = parents.asJava

  def bodyAsJava = body.asJava
}

case class This(qual: TypeName)(attachments: Attachments)
  extends AbstractTree(attachments) with TermTree

sealed trait RefTree extends NameTree {
  def qualifier: Tree
}

case class Ident(name: Name)(attachments: Attachments)
  extends AbstractTree(attachments) with RefTree {

  override def qualifier = EmptyTree
}

case class Select(qualifier: Tree, name: Name)(attachments: Attachments)
  extends AbstractTree(attachments) with RefTree

case class SelectFromTypeTree(qualifier: Tree, name: TypeName)(attachments: Attachments)
  extends AbstractTree(attachments) with RefTree with TypTree

sealed trait NameTree extends Tree {
  def name: Name
}

sealed trait TermTree extends Tree

case class Alternative(trees: List[Tree])(attachments: Attachments)
  extends AbstractTree(attachments) with TermTree {

  def treesAsJava = trees.asJava
}

case class Assign(lhs: Tree, rhs: Tree)(attachments: Attachments)
  extends AbstractTree(attachments) with TermTree

case class AssignOrNamedArg(lhs: Tree, rhs: Tree)(attachments: Attachments)
  extends AbstractTree(attachments) with TermTree

case class Block(stats: List[Tree], expr: Tree)(attachments: Attachments)
  extends AbstractTree(attachments) with TermTree {

  def statsAsJava = stats.asJava
}

sealed trait GenericApply extends TermTree {
  def fun: Tree

  def args: List[Tree]

  def argsAsJava = args.asJava
}

case class Apply(fun: Tree, args: List[Tree])(attachments: Attachments)
  extends AbstractTree(attachments) with GenericApply

case class TypeApply(fun: Tree, args: List[Tree])(attachments: Attachments)
  extends AbstractTree(attachments) with GenericApply

case class If(cond: Tree, thenp: Tree, elsep: Tree)(attachments: Attachments)
  extends AbstractTree(attachments) with TermTree

case class Literal(private val constant: Constant)(attachments: Attachments)
  extends AbstractTree(attachments) with TermTree {

  def value = constant.value
}

case class Constant(value: Any)(stringRepr: String) {
  override def toString = stringRepr
}

case class Match(selector: Tree, cases: List[CaseDef])(attachments: Attachments)
  extends AbstractTree(attachments) with TermTree {

  def casesAsJava = cases.asJava
}

case class New(tpt: Tree)(attachments: Attachments)
  extends AbstractTree(attachments) with TermTree

case class ReferenceToBoxed(ident: Ident)(attachments: Attachments)
  extends AbstractTree(attachments) with TermTree

case class Star(elem: Tree)(attachments: Attachments)
  extends AbstractTree(attachments) with TermTree

case class Super(qual: Tree, mix: TypeName)(attachments: Attachments)
  extends AbstractTree(attachments) with TermTree

case class Throw(expr: Tree)(attachments: Attachments)
  extends AbstractTree(attachments) with TermTree

case class Try(block: Tree, catches: List[CaseDef], finalizer: Tree)(attachments: Attachments)
  extends AbstractTree(attachments) with TermTree {

  def catchesAsJava = catches.asJava
}

case class Typed(expr: Tree, tpt: Tree)(attachments: Attachments)
  extends AbstractTree(attachments) with TermTree

case class UnApply(fun: Tree, args: List[Tree])(attachments: Attachments)
  extends AbstractTree(attachments) with TermTree {

  def argsAsJava = args.asJava
}

sealed abstract class EmptyTree extends AbstractTree(Attachments.empty) with TermTree

case object EmptyTree extends EmptyTree {
  def get: EmptyTree = this
}

sealed trait TypTree extends Tree

case class AppliedTypeTree(tpt: Tree, args: List[Tree])(attachments: Attachments)
  extends AbstractTree(attachments) with TypTree {

  def argsAsJava = args.asJava
}

case class CompoundTypeTree(templ: Template)(attachments: Attachments)
  extends AbstractTree(attachments) with TypTree

case class ExistentialTypeTree(tpt: Tree, whereClauses: List[Tree])(attachments: Attachments)
  extends AbstractTree(attachments) with TypTree {

  def whereClausesAsJava = whereClauses.asJava
}

case class SingletonTypeTree(ref: Tree)(attachments: Attachments)
  extends AbstractTree(attachments) with TypTree

case class TypeBoundsTree(lo: Tree, hi: Tree)(attachments: Attachments)
  extends AbstractTree(attachments) with TypTree

case class TypeTree()(val original: Tree, attachments: Attachments)
  extends AbstractTree(attachments) with TypTree
