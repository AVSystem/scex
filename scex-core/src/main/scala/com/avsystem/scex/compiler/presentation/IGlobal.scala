package com.avsystem.scex.compiler.presentation

import java.{util => ju, lang => jl}
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.{symtab, Settings}
import scala.tools.nsc.reporters.Reporter
import scala.collection.mutable
import symtab.Flags.{ACCESSOR, PARAMACCESSOR}
import scala.Some
import scala.reflect.internal.util.SourceFile

/**
 * Created: 13-12-2013
 * Author: ghik
 *
 * The compiler probably screws up tree positions when rewriting dynamic calls and because of that there's
 * literally no way to give the compiler a position under which it will find the appropriate tree.
 * As a result, I have to compute type completion manually instead of using [[askTypeCompletion]].
 * The code below is a modified implementation of the same functionality in [[scala.tools.nsc.interactive.Global]].
 */
class IGlobal(settings: Settings, reporter: Reporter) extends Global(settings, reporter) {

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
        if(symbol.isType) symbol.asType.toType.baseClasses match {
          case _ :: tail => tail.toSet
          case Nil => Set.empty
        } else Set.empty

      lazy val higherPriorityImplicit = m.implicitlyAdded && implicitlyAdded &&
        superclasses(implicitTree.symbol.owner).contains(m.implicitTree.symbol.owner)

      (m.sym.hasFlag(ACCESSOR | PARAMACCESSOR) && !sym.hasFlag(ACCESSOR | PARAMACCESSOR) &&
        (!implicitlyAdded || m.implicitlyAdded)) || higherPriorityImplicit
    }

    def add(sym: Symbol, pre: Type, implicitTree: Tree)(toMember: (Symbol, Type) => ScexTypeMember) {
      val implicitlyAdded = implicitTree != EmptyTree
      if ((sym.isGetter || sym.isSetter) && sym.accessed != NoSymbol) {
        add(sym.accessed, pre, implicitTree)(toMember)
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

    def allMembers: List[ScexTypeMember] = values.toList.flatten
  }

  def typeMembers(typedTree: Tree, pos: Position) = {
    var tree = typedTree

    val sym = Option(tree.symbol).getOrElse(NoSymbol)

    val context = doLocateContext(pos)

    if (tree.tpe == null)
      tree = analyzer.newTyper(context).typedQualifier(tree)

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

    val pre = stabilizedType(tree)

    val ownerTpe = tree.tpe match {
      case analyzer.ImportType(expr) => expr.tpe
      case null => pre
      case MethodType(List(), rtpe) => rtpe
      case _ => tree.tpe
    }

    for (sym <- ownerTpe.members)
      addTypeMember(sym, pre, sym.owner != ownerTpe.typeSymbol, EmptyTree, NoType)

    val applicableViews: List[analyzer.SearchResult] =
      if (ownerTpe.isErroneous) List()
      else new analyzer.ImplicitSearch(
        tree, definitions.functionType(List(ownerTpe), definitions.AnyClass.tpe), isView = true,
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

  class SilentCompilationUnit(source: SourceFile) extends CompilationUnit(source) {
    override def echo(pos: Position, msg: String) = ()

    override def error(pos: Position, msg: String) = ()

    override def warning(pos: Position, msg: String) = ()

    override def deprecationWarning(pos: Position, msg: String) = ()

    override def uncheckedWarning(pos: Position, msg: String) = ()

    override def inlinerWarning(pos: Position, msg: String) = ()

    override def incompleteInputError(pos: Position, msg: String) = ()

    override def comment(pos: Position, msg: String) = ()
  }

  override def parseTree(source: SourceFile) =
    new syntaxAnalyzer.UnitParser(new SilentCompilationUnit(source)).parse()
}
