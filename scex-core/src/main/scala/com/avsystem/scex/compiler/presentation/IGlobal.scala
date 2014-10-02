package com.avsystem.scex
package compiler.presentation

import java.{lang => jl, util => ju}

import com.avsystem.scex.compiler.ScexGlobal

import scala.collection.mutable
import scala.tools.nsc.Settings
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.symtab.Flags.{ACCESSOR, PARAMACCESSOR}

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
        add(sym.getterIn(sym.owner), pre, implicitTree)(toMember)
      } else if (!sym.name.decodedName.containsName("$") && !sym.isSynthetic && sym.hasRawInfo) {
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
      case Select(qual, name) if tree.tpe == null || tree.tpe == ErrorType =>
        tree = qual
      //workaround for another stupid scalac WTF regarding dynamics
      case Apply(Select(qual, TermName("selectDynamic")), List(lit@Literal(Constant("<error>")))) =>
        tree = qual
      case _ =>
    }

    val context = doLocateContext(pos)

    val shouldTypeQualifier = tree.tpe match {
      case null => true
      case mt: MethodType => mt.isImplicit
      case _ => false
    }

    if (shouldTypeQualifier) {
      tree = analyzer.newTyper(context).typedQualifier(tree)

      tree match {
        case Select(dyn, name) if (tree.tpe == null || tree.tpe == ErrorType) && dyn.tpe != null && dyn.tpe <:< typeOf[Dynamic] =>
          // why I have to do this manually is beyond me
          val literal = Literal(Constant(name.decoded)).setPos(tree.pos.withStart(dyn.pos.end + 1).makeTransparent)
          tree = Apply(Select(dyn, TermName("selectDynamic")).setPos(dyn.pos.withEnd(dyn.pos.end + 1)), List(literal)).setPos(tree.pos)
          tree = analyzer.newTyper(context).typedQualifier(tree)
        case _ =>
      }
    }

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
          context.isAccessible(if (s.hasGetter) s.getterIn(s.owner) else s, pre, superAccess && !implicitlyAdded),
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

    (tree, ownerTpe, members.allMembers)
  }

  // workaround for Scala parser bug that is a root cause of SI-8459
  override def newUnitParser(unit: CompilationUnit) =
    new syntaxAnalyzer.UnitParser(unit) {
      override def selector(t: Tree): Tree = {
        val point = if (isIdent) in.offset else in.lastOffset
        //assert(t.pos.isDefined, t)
        if (t != EmptyTree)
          Select(t, ident(skipIt = false)) setPos r2p(t.pos.start, point, in.lastOffset)
        else
          errorTermTree // has already been reported
      }
    }

}
