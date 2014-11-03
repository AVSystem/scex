package com.avsystem.scex
package compiler.presentation

import java.{lang => jl, util => ju}

import com.avsystem.scex.compiler.ScexCompiler.CompileError
import com.avsystem.scex.compiler.{ExpressionDef, _}
import com.avsystem.scex.parsing.EmptyPositionMapping
import com.avsystem.scex.util.CommonUtils._
import com.avsystem.scex.validation.ValidationContext

import scala.reflect.internal.util.SourceFile
import scala.reflect.runtime.universe.TypeTag

trait ScexPresentationCompiler extends ScexCompiler {
  compiler =>

  import com.avsystem.scex.compiler.presentation.ScexPresentationCompiler.{Completion, Param, Member => SMember, Type => SType}

  private val logger = createLogger[ScexPresentationCompiler]

  private object lock

  protected def underPresentationLock[T](code: => T) = {
    ensureSetup()
    lock.synchronized {
      code
    }
  }

  private var reporter: Reporter = _
  private var global: IGlobal = _

  override protected def setup(): Unit = {
    super.setup()
    lock.synchronized {
      logger.info("Initializing Scala presentation compiler")
      reporter = new Reporter(settings)
      global = new IGlobal(settings, reporter)
    }
  }

  def getOrThrow[T](resp: IGlobal#Response[T]) = resp.get match {
    case Left(res) => res
    case Right(t) => throw t
  }

  private def inCompilerThread[T](code: => T) = {
    getOrThrow(global.askForResponse(() => code))
  }

  protected final def withIGlobal[T](code: IGlobal => T) = underPresentationLock {
    reporter.reset()
    val global = compiler.global
    val result = try code(global) finally {
      reporter.reset()
    }
    result
  }

  class Completer(
    profile: ExpressionProfile,
    template: Boolean,
    setter: Boolean,
    header: String,
    contextType: String,
    rootObjectClass: Class[_],
    resultType: String) {

    require(profile != null, "Profile cannot be null")
    require(contextType != null, "Context type cannot be null")
    require(rootObjectClass != null, "Root object class cannot be null")
    require(resultType != null, "Result type cannot be null")

    private def exprDef(expression: String, bare: Boolean) = {
      val (actualExpression, positionMapping) =
        if(bare) (expression, EmptyPositionMapping) else preprocess(expression, template)

      ExpressionDef(profile, template && !bare, setter && !bare, actualExpression, header, contextType,
        if (bare) "Any" else resultType)(expression, positionMapping, rootObjectClass)
    }

    def getErrors(expression: String): List[CompileError] =
      compiler.getErrors(exprDef(expression, bare = false))

    def getScopeCompletion: Completion =
      compiler.getScopeCompletion(exprDef("()", bare = true))

    def getTypeCompletion(expression: String, position: Int): Completion =
      compiler.getTypeCompletion(exprDef(expression, bare = false), position)

    def parse(expression: String): ast.Tree =
      compiler.parse(exprDef(expression, bare = false))

  }

  def getCompleter[C <: ExpressionContext[_, _] : TypeTag, T: TypeTag](
    profile: ExpressionProfile,
    template: Boolean = true,
    setter: Boolean = false,
    header: String = ""): Completer = {

    import scala.reflect.runtime.universe._

    val mirror = typeTag[C].mirror
    val contextType = typeOf[C]
    val resultType = typeOf[T]
    val TypeRef(_, _, List(rootObjectType, _)) = contextType.baseType(typeOf[ExpressionContext[_, _]].typeSymbol)
    val rootObjectClass = mirror.runtimeClass(rootObjectType)

    getCompleter(profile, template, setter, header, contextType.toString, rootObjectClass, resultType.toString)
  }

  protected def getCompleter(
    profile: ExpressionProfile,
    template: Boolean,
    setter: Boolean,
    header: String,
    contextType: String,
    rootObjectClass: Class[_],
    resultType: String): Completer = {

    new Completer(profile, template, setter, header, contextType, rootObjectClass, resultType)
  }

  private def getContextTpe(global: IGlobal)(tree: global.Tree): global.Type = {
    import global._

    inCompilerThread {
      val PackageDef(_, List(ClassDef(_, _, _, Template(List(expressionParent, _), _, _)))) = tree
      val TypeRef(_, _, List(contextTpe, _)) = expressionParent.tpe
      contextTpe
    }
  }

  private def translateMember(global: IGlobal)(member: global.Member) = {
    import global._

    def translateType(tpe: Type) =
      tpe.toOpt.map { tpe =>
        SType(tpe.widen.toString(), erasureClass(tpe))
      }.orNull

    def symbolToParam(sym: Symbol) =
      Param(sym.decodedName, translateType(sym.typeSignature))

    SMember(member.sym.decodedName,
      paramsOf(member.tpe).map(_.map(symbolToParam)),
      translateType(resultTypeOf(member.tpe)),
      member.sym.isImplicit)
  }

  protected def getErrors(exprDef: ExpressionDef) = {
    val (pkgName, code, offset) = expressionCode(exprDef)
    val sourceFile = new ExpressionSourceFile(exprDef, pkgName, code, offset)
    withIGlobal { global =>
      val response = new global.Response[global.Tree]
      try {
        global.askLoadedTyped(sourceFile, response)
        getOrThrow(response)
        reporter.compileErrors()
      } finally {
        global.removeUnitOf(sourceFile)
      }
    }
  }

  protected def getScopeCompletion(exprDef: ExpressionDef): Completion = {
    val symbolValidator = exprDef.profile.symbolValidator

    val (pkgName, code, offset) = expressionCode(exprDef, noMacroProcessing = true)
    val sourceFile = new ExpressionSourceFile(exprDef, pkgName, code, offset)

    withIGlobal { global =>
      import global.{position => _, sourceFile => _, _}
      try {
        val pos = sourceFile.position(offset)
        logger.debug(s"Computing scope completion for $exprDef")

        val treeResponse = new Response[Tree]
        askLoadedTyped(sourceFile, treeResponse)
        val sourceTree = getOrThrow(treeResponse)

        val vc = ValidationContext(global)(getContextTpe(global)(sourceTree))
        import vc._

        def accessFromScopeMember(m: ScopeMember) = {
          // static module will be allowed by default only when at least one of its members is allowed
          val staticAccessAllowedByDefault = isStaticModule(m.sym) && symbolValidator.referencesModuleMember(m.sym.fullName)
          extractAccess(Select(m.viaImport, m.sym), staticAccessAllowedByDefault)
        }

        val response = new Response[List[Member]]
        askScopeCompletion(pos, response)
        val scope = getOrThrow(response)

        inCompilerThread {
          val membersIterator = scope.iterator.collect {
            case member@ScopeMember(sym, _, _, viaImport)
              if viaImport != EmptyTree && sym.isTerm && !sym.hasPackageFlag &&
                !isAdapterWrappedMember(sym) && (!isScexSynthetic(sym) || (isExpressionUtil(sym) && !isExpressionUtilObject(sym))) =>
              if (sym.hasGetter) member.copy(sym = sym.getterIn(sym.owner)) else member
          } filter { m =>
            symbolValidator.validateMemberAccess(vc)(accessFromScopeMember(m)).deniedAccesses.isEmpty
          } map translateMember(global)

          Completion(ast.EmptyTree, membersIterator.toVector)
        }

      } finally {
        removeUnitOf(sourceFile)
      }
    }
  }

  protected def getTypeCompletion(exprDef: ExpressionDef, position: Int) = {
    val symbolValidator = exprDef.profile.symbolValidator

    val (pkgName, code, offset) = expressionCode(exprDef, noMacroProcessing = true)
    val sourceFile = new ExpressionSourceFile(exprDef, pkgName, code, offset)

    withIGlobal { global =>
      import global.{position => _, sourceFile => _, _}
      try {
        val sourcePosition = sourceFile.position(offset + exprDef.positionMapping(position))

        logger.debug(s"Computing type completion for $exprDef at position $position")

        val treeResponse = new Response[Tree]
        askLoadedTyped(sourceFile, keepLoaded = true, treeResponse)
        val fullTree = getOrThrow(treeResponse)

        val vc = ValidationContext(global)(getContextTpe(global)(fullTree))
        import vc._

        inCompilerThread {
          // fix selectDynamic positions, which scalac computes incorrectly...
          object positionFixer extends Traverser {
            override def traverse(tree: Tree) = {
              super.traverse(tree)
              tree match {
                case tree@Apply(Select(_, TermName("selectDynamic")), List(lit@Literal(Constant(_: String))))
                  if lit.pos.isTransparent && lit.pos.end >= tree.pos.end =>
                  tree.setPos(tree.pos.withEnd(lit.pos.end))
                case _ =>
              }
              if (tree.pos.isRange) {
                def positions = (tree :: tree.children).iterator.map(_.pos).filter(_.isRange)
                val start = positions.map(_.start).min
                val end = positions.map(_.end).max
                val transparent = tree.pos.isTransparent
                tree.pos = tree.pos.withStart(start).withEnd(end)
                if (transparent) {
                  tree.pos = tree.pos.makeTransparent
                }
              }
            }
          }
          positionFixer.traverse(fullTree)

          val tree = new Locator(sourcePosition).locateIn(fullTree).toOpt
            .filter(t => t.pos != NoPosition && t.pos.start >= offset).getOrElse(EmptyTree)

          def fakeIdent(tpe: Type, symbol: Symbol) =
            Ident(nme.EMPTY).setSymbol(Option(symbol).getOrElse(NoSymbol)).setType(tpe)

          def isAllowed(tree: Tree) =
            symbolValidator.validateMemberAccess(vc)(extractAccess(tree)).deniedAccesses.isEmpty

          val validated = tree match {
            case Select(apply@ImplicitlyConverted(qual, fun), name) =>
              treeCopy.Select(tree, treeCopy.Apply(apply, fun, List(fakeIdent(qual.tpe, qual.symbol))), name)
            case Select(qual, name) =>
              treeCopy.Select(tree, fakeIdent(qual.tpe, qual.symbol), name)
            case _ =>
              EmptyTree
          }

          // predent type error on forbidden member selection
          if (!isAllowed(validated)) {
            tree.setType(ErrorType)
          }

          val (typedTree, ownerTpe, typeMembers) = global.typeMembers(tree, sourcePosition)

          logger.debug("Prefix tree for type completion:\n" + show(typedTree, printTypes = true, printPositions = true))

          val fakeDirectPrefix = fakeIdent(ownerTpe, tree.symbol)
          def fakeSelect(member: ScexTypeMember) = {
            val fakePrefix =
              if (!member.implicitlyAdded) fakeDirectPrefix
              else Apply(member.implicitTree, List(fakeDirectPrefix))
                .setSymbol(member.implicitTree.symbol).setType(member.implicitType)
            Select(fakePrefix, member.sym)
          }

          val members = typeMembers.collect {
            case m if m.sym.isTerm && m.sym.isPublic && !m.sym.isConstructor
              && !isAdapterWrappedMember(m.sym) && isAllowed(fakeSelect(m)) => translateMember(global)(m)
          }

          val translator = new ast.Translator(global, offset, exprDef)
          val translatedTree = translator.translateTree(typedTree.asInstanceOf[translator.u.Tree])

          Completion(translatedTree, members)
        }

      } finally {
        removeUnitOf(sourceFile)
      }
    }
  }

  protected def parse(exprDef: ExpressionDef) = withIGlobal { global =>
    val (parsedTree, _) = global.parseExpression(exprDef.expression, exprDef.template)

    inCompilerThread {
      val translator = new ast.Translator(global, 0, exprDef)
      translator.translateTree(parsedTree.asInstanceOf[translator.u.Tree])
    }
  }

  override protected def compile(sourceFile: ScexSourceFile) = {
    val result = super.compile(sourceFile)

    result match {
      case Left(_) if sourceFile.shared => underPresentationLock {
        val global = this.global
        val response = new global.Response[global.Tree]
        global.askLoadedTyped(sourceFile, response)
        getOrThrow(response)
      }
      case _ =>
    }

    result
  }

  override def reset(): Unit =
    underLock(underPresentationLock {
      global.askShutdown()
      super.reset()
    })
}

object ScexPresentationCompiler {

  case class Type(fullRepr: String, erasure: Class[_]) {
    override def toString = fullRepr
  }

  case class Param(name: String, tpe: Type)

  case class Member(name: String, params: List[List[Param]], tpe: Type, iimplicit: Boolean)

  case class Completion(typedPrefixTree: ast.Tree, members: Vector[Member])

}