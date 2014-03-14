package com.avsystem.scex
package compiler.presentation.ast

import java.{util => ju, lang => jl}
import com.avsystem.scex.util.CommonUtils
import scala.tools.nsc.Global
import com.avsystem.scex.compiler.ExpressionDef

/**
 * Created: 12-03-2014
 * Author: ghik
 */
class Translator(val u: Global, offset: Int, exprDef: ExpressionDef) {

  import CommonUtils._

  private val reverseMapping = exprDef.positionMapping.reverse

  def translateTree(tree: u.Tree) =
    translateTreeIn[Tree](tree)

  private object TemplateInterpolationTree {
    def unapply(tree: u.Apply) = tree match {
      case _ if tree.pos.startOrPoint >= offset => None
      case u.Apply(u.Select(StringContextApply(parts), _), args) => Some((parts, args))
      case _ => None
    }
  }

  object StringContextTree {
    def unapply(tree: u.Tree) = tree match {
      case u.Ident(name) if name.decoded == "StringContext" => true
      case u.Select(_, name) if name.decoded == "StringContext" => true
      case _ => false
    }
  }

  object StringContextApply {
    def unapply(tree: u.Tree) = tree match {
      case _ if !tree.pos.isTransparent => None
      case u.Apply(u.Select(StringContextTree(), name), parts) if name.decoded == "apply" => Some(parts)
      case u.Apply(StringContextTree(), parts) => Some(parts)
      case _ => None
    }
  }

  private def translateTreeIn[T <: Tree](tree: u.Tree): T = {
    lazy val attachments = translateAttachments(tree)

    val result = tree match {
      case null =>
        null
      case u.EmptyTree =>
        EmptyTree
      case TemplateInterpolationTree(parts, args) =>
        TemplateInterpolation(parts.map(translateTreeIn[Literal]), args.map(translateTree))(attachments)
      case u.Select(qualifier, name) =>
        Select(translateTree(qualifier), translateName(name))(attachments)
      case u.Ident(name) =>
        Ident(translateName(name))(attachments)
      case u.Apply(fun, args) =>
        Apply(translateTree(fun), args.map(translateTree))(attachments)
      case u.If(cond, thenp, elsep) =>
        If(translateTree(cond), translateTree(thenp), translateTree(elsep))(attachments)
      case u.Literal(const) =>
        Literal(translateConstant(const))(attachments)
      case u.Assign(lhs, rhs) =>
        Assign(translateTree(lhs), translateTree(rhs))(attachments)
      case u.Block(stats, expr) =>
        Block(stats.map(translateTree), translateTree(expr))(attachments)
      case u.TypeApply(fun, args) =>
        TypeApply(translateTree(fun), args.map(translateTree))(attachments)
      case u.Function(vparams, body) =>
        Function(vparams.map(translateTreeIn[ValDef]), translateTree(body))(attachments)
      case u.ValDef(mods, name, tpt, rhs) =>
        ValDef(translateModifiers(mods), translateTermName(name), translateTree(tpt), translateTree(rhs))(attachments)
      case u.New(tpt) =>
        New(translateTree(tpt))(attachments)
      case u.Annotated(annot, arg) =>
        Annotated(translateTree(annot), translateTree(arg))(attachments)
      case u.AssignOrNamedArg(lhs, rhs) =>
        AssignOrNamedArg(translateTree(lhs), translateTree(rhs))(attachments)
      case u.CaseDef(pat, guard, body) =>
        CaseDef(translateTree(pat), translateTree(guard), translateTree(body))(attachments)
      case u.Bind(name, body) =>
        Bind(translateName(name), translateTree(body))(attachments)
      case u.LabelDef(name, params, rhs) =>
        LabelDef(translateTermName(name), params.map(translateTreeIn[Ident]), translateTree(rhs))(attachments)
      case u.ClassDef(mods, name, tparams, impl) =>
        ClassDef(translateModifiers(mods), translateTypeName(name), tparams.map(translateTreeIn[TypeDef]), translateTreeIn[Template](impl))(attachments)
      case u.ModuleDef(mods, name, impl) =>
        ModuleDef(translateModifiers(mods), translateTermName(name), translateTreeIn[Template](tree))(attachments)
      case u.PackageDef(pid, stats) =>
        PackageDef(translateTreeIn[RefTree](pid), stats.map(translateTree))(attachments)
      case u.TypeDef(mods, name, tparams, rhs) =>
        TypeDef(translateModifiers(mods), translateTypeName(name), tparams.map(translateTreeIn[TypeDef]), translateTree(rhs))(attachments)
      case u.DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        DefDef(translateModifiers(mods), translateName(name), tparams.map(translateTreeIn[TypeDef]),
          vparamss.map(_.map(translateTreeIn[ValDef])), translateTree(tpt), translateTree(rhs))(attachments)
      case u.Import(expr, selectors) =>
        Import(translateTree(expr), selectors.map(translateImportSelector))(attachments)
      case u.Return(expr) =>
        Return(translateTree(expr))(attachments)
      case u.Template(parents, self, body) =>
        Template(parents.map(translateTree), translateTreeIn[ValDef](self), body.map(translateTree))(attachments)
      case u.This(qual) =>
        This(translateTypeName(qual))(attachments)
      case u.SelectFromTypeTree(qualifier, name) =>
        SelectFromTypeTree(translateTree(qualifier), translateTypeName(name))(attachments)
      case u.Alternative(trees) =>
        Alternative(trees.map(translateTree))(attachments)
      case u.Match(selector, cases) =>
        Match(translateTree(selector), cases.map(translateTreeIn[CaseDef]))(attachments)
      case u.ReferenceToBoxed(ident) =>
        ReferenceToBoxed(translateTreeIn[Ident](ident))(attachments)
      case u.Star(elem) =>
        Star(translateTree(elem))(attachments)
      case u.Super(qual, mix) =>
        Super(translateTree(qual), translateTypeName(mix))(attachments)
      case u.Throw(expr) =>
        Throw(translateTree(expr))(attachments)
      case u.Try(block, catches, finalizer) =>
        Try(translateTree(block), catches.map(translateTreeIn[CaseDef]), translateTree(finalizer))(attachments)
      case u.Typed(expr, tpt) =>
        Typed(translateTree(expr), translateTree(tpt))(attachments)
      case u.UnApply(fun, args) =>
        UnApply(translateTree(fun), args.map(translateTree))(attachments)
      case u.AppliedTypeTree(tpt, args) =>
        AppliedTypeTree(translateTree(tpt), args.map(translateTree))(attachments)
      case u.CompoundTypeTree(templ) =>
        CompoundTypeTree(translateTreeIn[Template](templ))(attachments)
      case u.ExistentialTypeTree(tpt, whereClauses) =>
        ExistentialTypeTree(translateTree(tpt), whereClauses.map(translateTree))(attachments)
      case u.SingletonTypeTree(ref) =>
        SingletonTypeTree(translateTree(ref))(attachments)
      case u.TypeBoundsTree(lo, hi) =>
        TypeBoundsTree(translateTree(lo), translateTree(hi))(attachments)
      case tt@u.TypeTree() =>
        TypeTree()(translateTree(tt.original), attachments)
      case _ =>
        throw new IllegalArgumentException(s"Unknown tree: ${u.showRaw(tree)}")
    }

    result.asInstanceOf[T]
  }

  def translateConstant(c: u.Constant) =
    if (c.tpe =:= u.typeOf[u.Type])
      Constant(translateSymbol(c.symbolValue))(c.escapedStringValue)
    else if (c.tpe =:= u.typeOf[u.Symbol])
      Constant(translateSymbol(c.symbolValue))(c.escapedStringValue)
    else
      Constant(c.value)(c.escapedStringValue)


  def translateAttachments(tree: u.Tree) = new Attachments(
    tree.tpe.toOpt.map(_.widen.toString()).getOrElse(null),
    translatePosition(tree.pos))

  def translateSymbol(sym: u.Symbol) = sym match {
    case null | u.NoSymbol => null
    case _ =>
      def translateNames(sym: u.Symbol): List[Name] = sym match {
        case null | u.NoSymbol => Nil
        case _ => translateName(sym.name) :: translateNames(sym.owner)
      }
      Symbol(translateNames(sym))
  }

  def translateName(name: u.Name) =
    if (name == null) null
    else if (name.isTermName) translateTermName(name.toTermName)
    else if (name.isTypeName) translateTypeName(name.toTypeName)
    else null

  def translateTermName(name: u.TermName) = name match {
    case u.nme.EMPTY => TermName.EMPTY
    case u.nme.ERROR => TermName.ERROR
    case u.nme.WILDCARD => TermName.WILDCARD
    case u.nme.PACKAGE => TermName.PACKAGE
    case u.nme.CONSTRUCTOR => TermName.CONSTRUCTOR
    case u.nme.ROOTPKG => TermName.ROOTPKG
    case _ => TermName(name.decoded)
  }

  def translateTypeName(name: u.TypeName) = name match {
    case u.tpnme.EMPTY => TypeName.EMPTY
    case u.tpnme.ERROR => TypeName.ERROR
    case u.tpnme.WILDCARD => TypeName.WILDCARD
    case u.tpnme.PACKAGE => TypeName.PACKAGE
    case u.tpnme.WILDCARD_STAR => TypeName.WILDCARD_STAR
    case _ => TypeName(name.decoded)
  }

  def translatePosition(pos: u.Position) = pos match {
    case u.NoPosition | null => null
    case _ => Position(
      reverseMapping(math.max(pos.startOrPoint, offset) - offset),
      reverseMapping(math.min(pos.endOrPoint, offset + exprDef.expression.length) - offset - 1) + 1,
      pos.isTransparent)
  }

  def translateModifiers(mods: u.Modifiers) =
    Modifiers(Flags(mods.flags)(mods.flagString), translateName(mods.privateWithin), mods.annotations.map(translateTree))

  def translateImportSelector(selector: u.ImportSelector) =
    ImportSelector(translateName(selector.name), selector.namePos, translateName(selector.rename), selector.renamePos)
}
