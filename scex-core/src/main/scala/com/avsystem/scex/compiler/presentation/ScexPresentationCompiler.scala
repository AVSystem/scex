package com.avsystem.scex
package compiler.presentation

import com.avsystem.scex.compiler._
import com.avsystem.scex.util.{CommonUtils, MacroUtils}
import com.avsystem.scex.validation.ValidationContext
import com.google.common.cache.CacheBuilder
import java.{util => ju, lang => jl}
import scala.reflect.internal.util.{SourceFile, BatchSourceFile}
import scala.reflect.runtime.universe.TypeTag
import com.avsystem.scex.compiler.ScexCompiler.CompileError
import com.avsystem.scex.compiler.ExpressionDef
import java.io.PrintWriter

trait ScexPresentationCompiler extends ScexCompiler {
  compiler =>

  import ScexPresentationCompiler.{Member => SMember, Param, Completion}
  import CommonUtils._

  private val logger = createLogger[ScexPresentationCompiler]

  private object lock

  private def underLock[T](code: => T) = lock.synchronized {
    if (!initialized) {
      init()
    }
    code
  }

  private var initialized = false

  private var reporter: Reporter = _
  private var global: IGlobal = _

  private def init(): Unit = {
    logger.info("Initializing Scala presentation compiler")
    reporter = new Reporter(settings)
    global = new IGlobal(settings, reporter)
    initialized = true
  }

  private def newInteractiveExpressionPackage() =
    newPackageName("_scex_interactive_expr")

  def getOrThrow[T](resp: IGlobal#Response[T]) = resp.get match {
    case Left(res) => res
    case Right(t) => throw t
  }

  private def inCompilerThread[T](code: => T) = {
    getOrThrow(global.askForResponse(() => code))
  }

  private def withGlobal[T](code: IGlobal => T) = underLock {
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
      val result = ExpressionDef(profile, template && !bare, setter && !bare, expression, PositionMapping.empty,
        header, rootObjectClass, contextType, if (bare) "Any" else resultType)
      if(bare) result else preprocess(result)
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
      val PackageDef(_, List(ClassDef(_, _, _, Template(List(_, expressionParent, _), _, _)))) = tree
      val TypeRef(_, _, List(contextTpe, _)) = expressionParent.tpe
      contextTpe
    }
  }

  private def translateMember(macroUtils: MacroUtils {val universe: IGlobal})(member: macroUtils.universe.Member) = {
    import macroUtils._
    import macroUtils.universe._

    def symbolToParam(sym: Symbol) =
      Param(sym.decodedName, sym.typeSignature.toString())

    SMember(member.sym.decodedName,
      paramsOf(member.tpe).map(_.map(symbolToParam)),
      resultTypeOf(member.tpe).toString(),
      member.sym.isImplicit)
  }

  protected def getErrors(exprDef: ExpressionDef) = withGlobal { global =>
    val pkgName = newInteractiveExpressionPackage()
    val (code, _) = expressionCode(exprDef, pkgName)
    val response = new global.Response[global.Tree]
    val sourceFile = new ExpressionSourceFile(exprDef.profile, pkgName, code)
    try {
      global.askLoadedTyped(sourceFile, response)
      getOrThrow(response)
      reporter.compileErrors()
    } finally {
      global.removeUnitOf(sourceFile)
    }
  }

  protected def getScopeCompletion(exprDef: ExpressionDef): Completion = withGlobal { global =>
    import global.{sourceFile => _, position => _, _}
    val pkgName = newInteractiveExpressionPackage()

    val symbolValidator = exprDef.profile.symbolValidator

    val (code, offset) = expressionCode(exprDef, pkgName, noMacroProcessing = true)
    val sourceFile = new ExpressionSourceFile(exprDef.profile, pkgName, code)

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
            if viaImport != EmptyTree && sym.isTerm && !sym.isPackage &&
              !isAdapterWrappedMember(sym) && (!isScexSynthetic(sym) || (isExpressionUtil(sym) && !isExpressionUtilObject(sym))) =>
            if (sym.hasGetter) member.copy(sym = sym.getter(sym.owner)) else member
        } filter { m =>
          symbolValidator.validateMemberAccess(vc)(accessFromScopeMember(m)).deniedAccesses.isEmpty
        } map translateMember(vc)

        Completion(ast.EmptyTree, membersIterator.toVector)
      }

    } finally {
      removeUnitOf(sourceFile)
    }
  }

  protected def getTypeCompletion(exprDef: ExpressionDef, position: Int) = withGlobal { global =>
    import global.{sourceFile => _, position => _, _}
    val symbolValidator = exprDef.profile.symbolValidator

    val pkgName = newInteractiveExpressionPackage()

    val (code, offset) = expressionCode(exprDef, pkgName, noMacroProcessing = true)
    val sourceFile = new ExpressionSourceFile(exprDef.profile, pkgName, code)

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
        fullTree.foreach {
          case tree@Apply(Select(_, TermName("selectDynamic")), List(lit@Literal(Constant(_: String))))
            if lit.pos.isTransparent => tree.setPos(tree.pos.withEnd(lit.pos.end))
          case _ =>
        }

        val tree = new Locator(sourcePosition).locateIn(fullTree).toOpt
          .filter(t => t.pos != NoPosition && t.pos.startOrPoint >= offset).getOrElse(EmptyTree)

        val (ownerTpe, typeMembers) = global.typeMembers(tree, sourcePosition)

        val fakeDirectPrefix = Ident(nme.EMPTY).setSymbol(Option(tree.symbol).getOrElse(NoSymbol)).setType(ownerTpe)
        def fakeSelect(member: ScexTypeMember) = {
          val fakePrefix =
            if (!member.implicitlyAdded) fakeDirectPrefix
            else Apply(member.implicitTree, List(fakeDirectPrefix))
              .setSymbol(member.implicitTree.symbol).setType(member.implicitType)
          Select(fakePrefix, member.sym)
        }

        def isMemberAllowed(member: ScexTypeMember) =
          symbolValidator.validateMemberAccess(vc)(extractAccess(fakeSelect(member))).deniedAccesses.isEmpty

        val members = typeMembers.collect {
          case m if m.sym.isTerm && m.sym.isPublic && !m.sym.isConstructor
            && !isAdapterWrappedMember(m.sym) && isMemberAllowed(m) => translateMember(vc)(m)
        }

        val translator = new ast.Translator(global, offset, exprDef)
        val translatedTree = translator.translateTree(tree.asInstanceOf[translator.u.Tree])

        Completion(translatedTree, members)
      }

    } finally {
      removeUnitOf(sourceFile)
    }
  }

  protected def parse(exprDef: ExpressionDef) = withGlobal { global =>
    val parsedTree = global.parseExpression(exprDef.expression, exprDef.template)

    inCompilerThread {
      val translator = new ast.Translator(global, 0, exprDef)
      translator.translateTree(parsedTree.asInstanceOf[translator.u.Tree])
    }
  }

  override protected def compile(sourceFile: SourceFile, classLoader: ScexClassLoader, usedInExpressions: Boolean) = {
    val result = super.compile(sourceFile, classLoader, usedInExpressions)

    if (result.isEmpty && usedInExpressions) {
      underLock {
        val global = this.global
        val response = new global.Response[global.Tree]
        global.askLoadedTyped(sourceFile, response)
        getOrThrow(response)
      }
    }

    result
  }

  override def reset() {
    underLock {
      synchronized {
        super.reset()
        global.askShutdown()
        init()
      }
    }
  }
}

object ScexPresentationCompiler {

  case class Param(name: String, tpe: String)

  case class Member(name: String, params: List[List[Param]], tpe: String, iimplicit: Boolean)

  case class Completion(typedPrefixTree: ast.Tree, members: Vector[Member])

}