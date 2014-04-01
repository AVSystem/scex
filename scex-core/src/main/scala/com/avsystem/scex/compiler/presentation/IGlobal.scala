package com.avsystem.scex
package compiler.presentation

import java.{util => ju, lang => jl}
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.{symtab, Settings}
import scala.tools.nsc.reporters.Reporter
import scala.collection.mutable
import symtab.Flags.{ACCESSOR, PARAMACCESSOR}
import scala.reflect.internal.util.{SourceFile, BatchSourceFile, TransparentPosition, RangePosition, OffsetPosition}
import com.avsystem.scex.compiler.{ScexGlobal, CodeGeneration}

/**
 * Created: 13-12-2013
 * Author: ghik
 *
 * I needed to hack a custom implementation of type completion, hence this class.
 */
class IGlobal(settings: Settings, reporter: Reporter) extends Global(settings, reporter) with ScexGlobal {

  import definitions._

  case class ScexTypeMember(
    sym: Symbol,
    tpe: Type,
    accessible: Boolean,
    inherited: Boolean,
    implicitTree: Tree,
    implicitType: Type) extends Member {
    override def implicitlyAdded = implicitTree != EmptyTree
  }

  private val Dollar = newTermName("$")

  private class Members extends mutable.LinkedHashMap[Name, Set[ScexTypeMember]] {
    override def default(key: Name) = Set()

    private def matching(sym: Symbol, symtpe: Type, ms: Set[ScexTypeMember]): Option[ScexTypeMember] = ms.find { m =>
      (m.sym.name == sym.name) && (m.sym.isType || (m.tpe matches symtpe))
    }

    private def keepSecond(m: ScexTypeMember, sym: Symbol, implicitTree: Tree): Boolean = {
      val implicitlyAdded = implicitTree != EmptyTree
      def superclasses(symbol: Symbol): Set[Symbol] =
        if (symbol.isType) symbol.asType.toType.baseClasses match {
          case _ :: tail => tail.toSet
          case Nil => Set.empty
        } else Set.empty

      lazy val higherPriorityImplicit = m.implicitlyAdded && implicitlyAdded &&
        superclasses(implicitTree.symbol.owner).contains(m.implicitTree.symbol.owner)

      (m.sym.hasFlag(ACCESSOR | PARAMACCESSOR) && !sym.hasFlag(ACCESSOR | PARAMACCESSOR) &&
        (!implicitlyAdded || m.implicitlyAdded)) || higherPriorityImplicit
    }

    def add(sym: Symbol, pre: Type, implicitTree: Tree)(toMember: (Symbol, Type) => ScexTypeMember) {
      if (sym.hasGetter) {
        add(sym.getter(sym.owner), pre, implicitTree)(toMember)
      } else if (!sym.name.decodedName.containsName(Dollar) && !sym.isSynthetic && sym.hasRawInfo) {
        val symtpe = pre.memberType(sym) onTypeError ErrorType
        matching(sym, symtpe, this(sym.name)) match {
          case Some(m) =>
            if (keepSecond(m, sym, implicitTree)) {
              this(sym.name) = this(sym.name) - m + toMember(sym, symtpe)
            }
          case None =>
            this(sym.name) = this(sym.name) + toMember(sym, symtpe)
        }
      }
    }

    def addNonShadowed(other: Members) = {
      for ((name, ms) <- other)
        if (ms.nonEmpty && this(name).isEmpty) this(name) = ms
    }

    def allMembers: Vector[ScexTypeMember] = values.toVector.flatten
  }

  def typeMembers(typedTree: Tree, pos: Position) = {
    var tree = typedTree

    // if tree consists of just x. or x.fo where fo is not yet a full member name
    // ignore the selection and look in just x.
    tree match {
      case Select(qual, name) if tree.tpe == ErrorType => tree = qual
      case _ =>
    }

    val sym = Option(tree.symbol).getOrElse(NoSymbol)

    val context = doLocateContext(pos)

    val shouldTypeQualifier = tree.tpe match {
      case null => true
      case mt: MethodType => mt.isImplicit
      case _ => false
    }

    if (shouldTypeQualifier)
      tree = analyzer.newTyper(context).typedQualifier(tree)

    val pre = stabilizedType(tree)

    val ownerTpe = tree.tpe match {
      case analyzer.ImportType(expr) => expr.tpe
      case null => pre
      case MethodType(List(), rtpe) => rtpe
      case _ => tree.tpe
    }

    val superAccess = tree.isInstanceOf[Super]
    val members = new Members

    def addTypeMember(sym: Symbol, pre: Type, inherited: Boolean, implicitTree: Tree, implicitType: Type) = {
      val implicitlyAdded = implicitTree != EmptyTree
      members.add(sym, pre, implicitTree) { (s, st) =>
        new ScexTypeMember(s, st,
          context.isAccessible(if (s.hasGetter) s.getter(s.owner) else s, pre, superAccess && !implicitlyAdded),
          inherited, implicitTree, implicitType)
      }
    }

    /** Create a function application of a given view function to `tree` and typecheck it.
      */
    def viewApply(view: analyzer.SearchResult): Tree = {
      assert(view.tree != EmptyTree)
      analyzer.newTyper(context.makeImplicit(reportAmbiguousErrors = false))
        .typed(Apply(view.tree, List(tree)) setPos tree.pos)
        .onTypeError(EmptyTree)
    }

    for (sym <- ownerTpe.members)
      addTypeMember(sym, pre, sym.owner != ownerTpe.typeSymbol, EmptyTree, NoType)

    val applicableViews: List[analyzer.SearchResult] =
      if (ownerTpe.isErroneous || ownerTpe <:< NullTpe || ownerTpe <:< NothingTpe) List()
      else new analyzer.ImplicitSearch(
        tree, functionType(List(ownerTpe), AnyClass.tpe), isView = true,
        context0 = context.makeImplicit(reportAmbiguousErrors = false)).allImplicits

    for (view <- applicableViews) {
      val vtree = viewApply(view)
      val vpre = stabilizedType(vtree)
      for (sym <- vtree.tpe.members) {
        addTypeMember(sym, vpre, inherited = false, view.tree, vpre)
      }
    }

    (ownerTpe, members.allMembers)
  }

}
