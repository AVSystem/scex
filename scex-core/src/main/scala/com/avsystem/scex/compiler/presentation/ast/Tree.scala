package com.avsystem.scex
package compiler.presentation.ast

import com.avsystem.commons.jiop.JavaInterop._

sealed trait Tree extends PrettyPrint {

  import com.avsystem.scex.util.CommonUtils._

  def attachments: Attachments

  lazy val children: List[Tree] = {
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
      def traverse(tree: Tree): Unit = {
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
    def pretty(indent: Int, obj: Any): Unit = {
      val newline = "\n" + "  " * indent
      def prettyElements(prefix: String, elems: Vector[Any]): Unit = {
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
final case class TemplateInterpolation(parts: List[Literal], args: List[Tree])(attachments: Attachments)
  extends AbstractTree(attachments) with TermTree {

  def partsAsJava = parts.asJava

  def argsAsJava = args.asJava
}

final case class Annotated(annot: Tree, arg: Tree)(attachments: Attachments)
  extends AbstractTree(attachments)

final case class CaseDef(pat: Tree, guard: Tree, body: Tree)(attachments: Attachments)
  extends AbstractTree(attachments)

sealed trait DefTree extends NameTree

final case class Bind(name: Name, body: Tree)(attachments: Attachments)
  extends AbstractTree(attachments) with DefTree

final case class LabelDef(name: TermName, params: List[Ident], rhs: Tree)(attachments: Attachments)
  extends AbstractTree(attachments) with DefTree with TermTree {

  def paramsAsJava: JList[Ident] = params.asJava
}

sealed trait MemberDef extends DefTree {
  def mods: Modifiers
}

sealed trait ImplDef extends MemberDef {
  def impl: Template
}

final case class ClassDef(mods: Modifiers, name: TypeName, tparams: List[TypeDef], impl: Template)(attachments: Attachments)
  extends AbstractTree(attachments) with ImplDef {

  def tparamsAsJava: JList[TypeDef] = tparams.asJava
}

final case class ModuleDef(mods: Modifiers, name: TermName, impl: Template)(attachments: Attachments)
  extends AbstractTree(attachments) with ImplDef

final case class PackageDef(pid: RefTree, stats: List[Tree])(attachments: Attachments)
  extends AbstractTree(attachments) with MemberDef {

  def name = pid.name

  def mods = Modifiers.empty

  def statsAsJava = stats.asJava
}

final case class TypeDef(mods: Modifiers, name: TypeName, tparams: List[TypeDef], rhs: Tree)(attachments: Attachments)
  extends AbstractTree(attachments) with MemberDef {

  def tparamsAsJava: JList[TypeDef] = tparams.asJava
}

sealed trait ValOrDefDef extends MemberDef {
  def tpt: Tree

  def rhs: Tree
}

final case class DefDef(mods: Modifiers, name: Name, tparams: List[TypeDef], vparamss: List[List[ValDef]],
  tpt: Tree, rhs: Tree)(attachments: Attachments) extends AbstractTree(attachments) with ValOrDefDef {

  def tparamsAsJava: JList[TypeDef] = tparams.asJava

  def vparamssAsJava: JList[JList[ValDef]] = vparamss.map(_.asJava).asJava
}

final case class ValDef(mods: Modifiers, name: TermName, tpt: Tree, rhs: Tree)(attachments: Attachments)
  extends AbstractTree(attachments) with ValOrDefDef

final case class Function(vparams: List[ValDef], body: Tree)(attachments: Attachments)
  extends AbstractTree(attachments) with TermTree {

  def vparamsAsJava: JList[ValDef] = vparams.asJava
}

final case class ImportSelector(name: Name, namePos: Int, rename: Name, renamePos: Int)

final case class Import(expr: Tree, selectors: List[ImportSelector])(attachments: Attachments)
  extends AbstractTree(attachments) {

  def selectorsAsJava = selectors.asJava
}

final case class Return(expr: Tree)(attachments: Attachments)
  extends AbstractTree(attachments) with TermTree

final case class Template(parents: List[Tree], self: ValDef, body: List[Tree])(attachments: Attachments)
  extends AbstractTree(attachments) {

  def parentsAsJava = parents.asJava

  def bodyAsJava = body.asJava
}

final case class This(qual: TypeName)(attachments: Attachments)
  extends AbstractTree(attachments) with TermTree

sealed trait RefTree extends NameTree {
  def qualifier: Tree
}

final case class Ident(name: Name)(attachments: Attachments)
  extends AbstractTree(attachments) with RefTree {

  override def qualifier = EmptyTree
}

final case class Select(qualifier: Tree, name: Name)(attachments: Attachments)
  extends AbstractTree(attachments) with RefTree

final case class SelectFromTypeTree(qualifier: Tree, name: TypeName)(attachments: Attachments)
  extends AbstractTree(attachments) with RefTree with TypTree

sealed trait NameTree extends Tree {
  def name: Name
}

sealed trait TermTree extends Tree

final case class Alternative(trees: List[Tree])(attachments: Attachments)
  extends AbstractTree(attachments) with TermTree {

  def treesAsJava: JList[Tree] = trees.asJava
}

final case class Assign(lhs: Tree, rhs: Tree)(attachments: Attachments)
  extends AbstractTree(attachments) with TermTree

final case class NamedArg(lhs: Tree, rhs: Tree)(attachments: Attachments)
  extends AbstractTree(attachments) with TermTree

final case class Block(stats: List[Tree], expr: Tree)(attachments: Attachments)
  extends AbstractTree(attachments) with TermTree {

  def statsAsJava: JList[Tree] = stats.asJava
}

sealed trait GenericApply extends TermTree {
  def fun: Tree

  def args: List[Tree]

  def argsAsJava: JList[Tree] = args.asJava
}

final case class Apply(fun: Tree, args: List[Tree])(attachments: Attachments)
  extends AbstractTree(attachments) with GenericApply

final case class TypeApply(fun: Tree, args: List[Tree])(attachments: Attachments)
  extends AbstractTree(attachments) with GenericApply

final case class If(cond: Tree, thenp: Tree, elsep: Tree)(attachments: Attachments)
  extends AbstractTree(attachments) with TermTree

final case class Literal(private val constant: Constant)(attachments: Attachments)
  extends AbstractTree(attachments) with TermTree {

  def value = constant.value
}

final case class Constant(value: Any)(stringRepr: String) {
  override def toString = stringRepr
}

final case class Match(selector: Tree, cases: List[CaseDef])(attachments: Attachments)
  extends AbstractTree(attachments) with TermTree {

  def casesAsJava: JList[CaseDef] = cases.asJava
}

final case class New(tpt: Tree)(attachments: Attachments)
  extends AbstractTree(attachments) with TermTree

final case class ReferenceToBoxed(ident: Ident)(attachments: Attachments)
  extends AbstractTree(attachments) with TermTree

final case class Star(elem: Tree)(attachments: Attachments)
  extends AbstractTree(attachments) with TermTree

final case class Super(qual: Tree, mix: TypeName)(attachments: Attachments)
  extends AbstractTree(attachments) with TermTree

final case class Throw(expr: Tree)(attachments: Attachments)
  extends AbstractTree(attachments) with TermTree

final case class Try(block: Tree, catches: List[CaseDef], finalizer: Tree)(attachments: Attachments)
  extends AbstractTree(attachments) with TermTree {

  def catchesAsJava: JList[CaseDef] = catches.asJava
}

final case class Typed(expr: Tree, tpt: Tree)(attachments: Attachments)
  extends AbstractTree(attachments) with TermTree

final case class UnApply(fun: Tree, args: List[Tree])(attachments: Attachments)
  extends AbstractTree(attachments) with TermTree {

  def argsAsJava: JList[Tree] = args.asJava
}

sealed abstract class EmptyTree extends AbstractTree(Attachments.empty) with TermTree

case object EmptyTree extends EmptyTree {
  def get: EmptyTree = this
}

sealed trait TypTree extends Tree

final case class AppliedTypeTree(tpt: Tree, args: List[Tree])(attachments: Attachments)
  extends AbstractTree(attachments) with TypTree {

  def argsAsJava: JList[Tree] = args.asJava
}

final case class CompoundTypeTree(templ: Template)(attachments: Attachments)
  extends AbstractTree(attachments) with TypTree

final case class ExistentialTypeTree(tpt: Tree, whereClauses: List[Tree])(attachments: Attachments)
  extends AbstractTree(attachments) with TypTree {

  def whereClausesAsJava: JList[Tree] = whereClauses.asJava
}

final case class SingletonTypeTree(ref: Tree)(attachments: Attachments)
  extends AbstractTree(attachments) with TypTree

final case class TypeBoundsTree(lo: Tree, hi: Tree)(attachments: Attachments)
  extends AbstractTree(attachments) with TypTree

final case class TypeTree()(val original: Tree, attachments: Attachments)
  extends AbstractTree(attachments) with TypTree
