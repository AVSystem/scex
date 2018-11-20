package com.avsystem.scex
package compiler.presentation

import com.avsystem.scex.compiler.ScexGlobal

import scala.collection.mutable
import scala.tools.nsc.Settings
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.symtab.Flags.{ACCESSOR, PARAMACCESSOR}

/**
  * I needed to hack a custom implementation of completion, hence this class.
  */
class IGlobal(settings: Settings, reporter: Reporter, val classLoader: ClassLoader)
  extends Global(settings, reporter) with ScexGlobal {

  import definitions._

  abstract class ScexMember extends Member {
    def prefix: Type
    def ownerTpe: Type = prefix
    def implicitTree: Tree
    def implicitType: Type

    override def implicitlyAdded: Boolean = implicitTree != EmptyTree
  }

  case class ScexTypeMember(
    prefix: Type,
    sym: Symbol,
    tpe: Type,
    accessible: Boolean,
    implicitTree: Tree,
    implicitType: Type) extends ScexMember

  case class ScexScopeMember(
    sym: Symbol,
    tpe: Type,
    accessible: Boolean,
    viaImport: Tree
  ) extends ScexMember {
    def prefix: Type = viaImport.tpe
    def implicitTree: Tree = EmptyTree
    def implicitType: Type = NoType
  }

  private class Members[T <: ScexMember] extends mutable.LinkedHashMap[Name, Set[T]] {
    override def default(key: Name) = Set()

    private def matching(sym: Symbol, symtpe: Type, ms: Set[T]): Option[T] = ms.find { m =>
      (m.sym.name == sym.name) && (m.sym.isType || (m.tpe matches symtpe))
    }

    private def keepSecond(m: T, sym: Symbol, implicitTree: Tree): Boolean = {
      val implicitlyAdded = implicitTree != EmptyTree

      def superclasses(symbol: Symbol): Set[Symbol] =
        if (symbol.isType) symbol.asType.toType.baseClasses match {
          case _ :: tail => tail.toSet
          case Nil => Set.empty
        } else Set.empty

      def argumentType(tpe: Type) = tpe match {
        case MethodType(List(param), _) => param.typeSignature
        case _ => NoType
      }

      def compare[A](a1: A, a2: A)(f: (A, A) => Boolean) =
        (if (f(a1, a2)) 1 else 0) + (if (f(a2, a1)) -1 else 0)

      lazy val higherPriorityImplicit = m.implicitlyAdded && implicitlyAdded && {
        val specificityPoints = compare(argumentType(implicitTree.tpe), argumentType(m.implicitTree.tpe))(_ <:< _)
        val priorityPoints = compare(implicitTree.symbol.owner, m.implicitTree.symbol.owner)((s1, s2) => superclasses(s1).contains(s2))
        specificityPoints + priorityPoints > 0
      }

      (m.sym.hasFlag(ACCESSOR | PARAMACCESSOR) && !sym.hasFlag(ACCESSOR | PARAMACCESSOR) &&
        (!implicitlyAdded || m.implicitlyAdded)) || higherPriorityImplicit
    }

    def add(sym: Symbol, pre: Type, implicitTree: Tree)(toMember: (Symbol, Type) => T): Unit = {
      if (sym.hasGetter) {
        add(sym.getterIn(sym.owner), pre, implicitTree)(toMember)
      } else if (!sym.name.decodedName.containsName("$") && !sym.isError && !sym.isArtifact && sym.hasRawInfo) {
        val symtpe = pre.memberType(sym) onTypeError ErrorType
        matching(sym, symtpe, this (sym.name)) match {
          case Some(m) =>
            if (keepSecond(m, sym, implicitTree)) {
              this (sym.name) = this (sym.name) - m + toMember(sym, symtpe)
            }
          case None =>
            this (sym.name) = this (sym.name) + toMember(sym, symtpe)
        }
      }
    }

    def addNonShadowed(other: Members[T]): Unit = {
      for ((name, ms) <- other)
        if (ms.nonEmpty && this (name).isEmpty) this (name) = ms
    }

    def allMembers: Vector[T] = values.toVector.flatten
  }

  // impl copied from interactive.Global and adjusted
  /** Return all members visible without prefix in context enclosing `pos`. */
  def scopeMembers(pos: Position): Vector[ScexScopeMember] = {
    val context = doLocateContext(pos)
    val locals = new Members[ScexScopeMember]
    val enclosing = new Members[ScexScopeMember]

    def addScopeMember(sym: Symbol, pre: Type, viaImport: Tree): Unit =
      locals.add(sym, pre, EmptyTree) { (s, st) =>
        // imported val and var are always marked as inaccessible, but they could be accessed through their getters. scala/bug#7995
        val actualSym = if (s.hasGetter) s.getterIn(s.owner) else s
        ScexScopeMember(actualSym, st, context.isAccessible(actualSym, pre, superAccess = false), viaImport)
      }

    def localsToEnclosing(): Unit = {
      enclosing.addNonShadowed(locals)
      locals.clear()
    }

    var cx = context
    while (cx.prefix.typeSymbol != RootClass) { // don't include toplevel packages into scope completion
      for (sym <- cx.scope)
        addScopeMember(sym, NoPrefix, EmptyTree)
      localsToEnclosing()
      if (cx == cx.enclClass) {
        val pre = cx.prefix
        for (sym <- pre.members)
          addScopeMember(sym, pre, EmptyTree)
        localsToEnclosing()
      }
      cx = cx.outer
    }
    for (imp <- context.imports) {
      val pre = imp.qual.tpe
      for (sym <- imp.allImportedSymbols)
        addScopeMember(sym, pre, imp.qual)
      localsToEnclosing()
    }
    enclosing.allMembers
  }

  case class TypeCompletionContext(context: Context, prefixTree: Tree, pre: Type, ownerTpe: Type)

  object ErroneousSelectDynamic {
    def unapply(tree: Tree): Option[(Tree, Literal)] = tree match {
      case Select(qual, name)
        if (tree.tpe == ErrorType || tree.tpe == null) && qual.tpe != null && qual.tpe != ErrorType && qual.tpe <:< dynamicTpe =>
        val literalPos = tree.pos.withStart(tree.pos.end min (qual.pos.end + 1)).makeTransparent
        val literal = Literal(Constant(name.decoded)).setPos(literalPos)
        Some((qual, literal))
      case _ =>
        None
    }
  }

  def typeCompletionContext(typedTree: Tree, pos: Position): TypeCompletionContext = {
    val context = doLocateContext(pos)
    var tree = typedTree

    // apparently, in some cases with dynamics, the tree comes completely untyped

    if (tree.tpe == null) {
      tree = analyzer.newTyper(context).typedQualifier(tree)
    }

    // now, drop selection being edited or assume empty tree when not editing a selection

    def includes(tree: Tree, inner: Position): Boolean =
      if (tree.pos.isDefined && !tree.pos.isTransparent)
        tree.pos.start <= inner.pos.start && tree.pos.end >= inner.pos.end
      else
        tree.children.exists(includes(_, inner))

    tree match {
      case SyntacticIdent(_) | Literal(_) | VariableIdent(_) if includes(tree, pos) =>
        tree = EmptyTree
      case SyntacticSelect(qual, _) if includes(tree, pos) =>
        tree = qual
      case _ =>
        tree = EmptyTree
    }

    // manually help the compiler understand that the qualifier is a proper dynamic call

    def retypeQual(tree: Tree) =
      analyzer.newTyper(context).typedQualifier(tree)

    def fixDynamicCalls(tree: Tree): Tree = tree match {
      case Select(qual, name) if qual.tpe == null =>
        fixDynamicCalls(retypeQual(Select(retypeQual(qual), name).setPos(tree.pos)))

      case Select(qual, name) if tree.tpe == null || tree.tpe == ErrorType =>
        val preFixed =
          if (qual.tpe == ErrorType)
            retypeQual(Select(fixDynamicCalls(qual), name).setPos(tree.pos))
          else tree

        preFixed match {
          case ErroneousSelectDynamic(newQual, literal) =>
            retypeQual(Apply(Select(newQual, TermName("selectDynamic")).setPos(newQual.pos), List(literal)).setPos(tree.pos))
          case _ =>
            preFixed
        }
      case _ => tree
    }

    tree = fixDynamicCalls(tree)

    // possibly retype to catch up with changes made to the tree

    val shouldRetypeQualifier = tree.tpe match {
      case null => true
      case mt: MethodType => mt.isImplicit || mt.params.isEmpty
      case _ => false
    }

    if (shouldRetypeQualifier) {
      tree = analyzer.newTyper(context).typedQualifier(tree)
    }

    // remove dangling implicit conversion

    tree match {
      case ImplicitlyConverted(qual, _) =>
        tree = qual
      case _ =>
    }

    val pre = stabilizedType(tree)

    val ownerTpe = tree.tpe match {
      case analyzer.ImportType(expr) => expr.tpe
      case null => pre
      case MethodType(List(), rtpe) => rtpe
      case _ => tree.tpe
    }

    TypeCompletionContext(context, tree, pre, ownerTpe)
  }

  /**
    * Reimplementation of `scala.tools.interactive.Global.typeMembers` method, adjusted to SCEX needs:
    * <ul>
    * <li>returned completion members contain more information (e.g. implicit view tree instead of just symbol)</li>
    * <li>there is a number of hacks and workarounds for scalac inability to properly handle dynamic invocations</li>
    * <li>all members are returned at once, instead of returning a stream</li>
    * </ul>
    */
  def typeMembers(completionContext: TypeCompletionContext): Vector[ScexTypeMember] = {
    val TypeCompletionContext(context, tree, pre, ownerTpe) = completionContext

    val superAccess = tree.isInstanceOf[Super]
    val members = new Members[ScexTypeMember]

    def addTypeMember(sym: Symbol, pre: Type, implicitTree: Tree, implicitType: Type): Unit = {
      val implicitlyAdded = implicitTree != EmptyTree
      members.add(sym, pre, implicitTree) { (s, st) =>
        ScexTypeMember(ownerTpe, s, st,
          context.isAccessible(if (s.hasGetter) s.getterIn(s.owner) else s, pre, superAccess && !implicitlyAdded),
          implicitTree, implicitType)
      }
    }

    /*
     * Create a function application of a given view function to `tree` and typecheck it.
     */
    def viewApply(view: analyzer.SearchResult): Tree = {
      assert(view.tree != EmptyTree)
      analyzer.newTyper(context.makeImplicit(reportAmbiguousErrors = false))
        .typed(Apply(view.tree, List(tree)) setPos tree.pos)
        .onTypeError(EmptyTree)
    }

    for (sym <- ownerTpe.members)
      addTypeMember(sym, pre, EmptyTree, NoType)

    val applicableViews: List[analyzer.SearchResult] =
      if (ownerTpe.isErroneous || ownerTpe <:< NullTpe || ownerTpe <:< NothingTpe) List()
      else new analyzer.ImplicitSearch(
        tree, functionType(List(ownerTpe), AnyClass.tpe), isView = true,
        context0 = context.makeImplicit(reportAmbiguousErrors = false)).allImplicits

    for (view <- applicableViews) {
      val vtree = viewApply(view)
      val vpre = stabilizedType(vtree)
      for (sym <- vtree.tpe.members) {
        addTypeMember(sym, vpre, view.tree, vpre)
      }
    }

    members.allMembers
  }

}
