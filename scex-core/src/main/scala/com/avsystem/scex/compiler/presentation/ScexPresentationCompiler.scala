package com.avsystem.scex
package compiler.presentation

import com.avsystem.scex.compiler.{CodeGeneration, ExpressionMacroProcessor, ScexCompiler}
import com.avsystem.scex.util.MacroUtils
import com.avsystem.scex.validation.ValidationContext
import com.google.common.cache.CacheBuilder
import java.{util => ju, lang => jl}
import scala.reflect.internal.util.{SourceFile, BatchSourceFile}
import scala.reflect.runtime.universe.TypeTag
import scala.tools.nsc.interactive.{Global => IGlobal}
import com.avsystem.scex.compiler.ScexCompiler.CompileError
import com.avsystem.scex.compiler.ExpressionDef

trait ScexPresentationCompiler extends ScexCompiler {
  compiler =>

  import ScexPresentationCompiler.{Member => SMember, Param, Completion}

  private val logger = createLogger[ScexPresentationCompiler]

  private object lock

  @inline
  private def underLock[T](code: => T) = {
    lock.synchronized {
      code
    }
  }

  private var reporter: Reporter = _
  private var global: IGlobal = _

  private val typeCompletionCache =
    CacheBuilder.newBuilder.maximumSize(100).build[(Completer, TypeWrapper), List[SMember]]

  private def init(): Unit = {
    logger.info("Initializing Scala presentation compiler")
    reporter = new Reporter(settings)
    global = new IGlobal(settings, reporter)
  }

  init()

  private def newInteractiveExpressionPackage() =
    newPackageName("_scex_interactive_expr")

  def getOrThrow[T](resp: IGlobal#Response[T]) = resp.get match {
    case Left(res) => res
    case Right(t) => throw t
  }

  trait Completer {
    def getErrors(expression: String): List[CompileError]

    def getScopeCompletion: Completion

    def getTypeCompletion(expression: String, position: Int): Completion
  }

  private def inCompilerThread[T](code: => T) = {
    getOrThrow(global.askForResponse(() => code))
  }

  private def withGlobal[T](profile: ExpressionProfile)(code: IGlobal => T) = underLock {
    val global = compiler.global
    inCompilerThread {
      ExpressionMacroProcessor.profileVar.value = profile
    }
    val result = try code(global) finally {
      inCompilerThread {
        ExpressionMacroProcessor.profileVar.value = null
      }
      reporter.reset()
    }
    result
  }

  private class CompleterImpl(
    profile: ExpressionProfile,
    template: Boolean,
    setter: Boolean,
    header: String,
    contextType: String,
    rootObjectClass: Class[_],
    resultType: String) extends Completer {

    require(profile != null, "Profile cannot be null")
    require(contextType != null, "Context type cannot be null")
    require(rootObjectClass != null, "Root object class cannot be null")
    require(resultType != null, "Result type cannot be null")

    private def exprDef(expression: String, resultType: String) =
      ExpressionDef(profile, template, setter, expression, header, rootObjectClass, contextType, resultType)

    def getErrors(expression: String): List[CompileError] =
      compiler.getErrors(exprDef(expression, resultType))

    def getScopeCompletion: Completion =
      compiler.getScopeCompletion(exprDef("()", "Any"))

    def getTypeCompletion(expression: String, position: Int): Completion =
      compiler.getTypeCompletion(exprDef(getSubExpression(expression, position), "Any"))
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

    new CompleterImpl(profile, template, setter, header, contextType, rootObjectClass, resultType)
  }

  protected def getSubExpression(expression: String, position: Int) = {
    val global = compiler.global
    import global.{sourceFile => _, position => _, _}

    val (code, offset) = CodeGeneration.wrapForParsing(expression)
    val sourceFile = new BatchSourceFile("(for_parsing)", code)
    val sourcePosition = sourceFile.position(offset + position)
    val PackageDef(_, List(ModuleDef(_, _, Template(_, _, List(_, expressionTree))))) = parseTree(sourceFile)

    val subtree = new Locator(sourcePosition).locateIn(expressionTree) match {
      case EmptyTree => expressionTree
      case Select(qualifier, nme.ERROR) => qualifier
      case t => t
    }

    subtree.toString()
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

  protected def getErrors(exprDef: ExpressionDef) = withGlobal(exprDef.profile) { global =>
    val pkgName = newInteractiveExpressionPackage()
    val (code, _) = expressionCode(exprDef, pkgName)
    val response = new global.Response[global.Tree]
    global.askLoadedTyped(new BatchSourceFile(pkgName, code), response)
    getOrThrow(response)
    reporter.compileErrors()
  }

  protected def getScopeCompletion(exprDef: ExpressionDef): Completion = withGlobal(exprDef.profile) { global =>
    import global.{sourceFile => _, position => _, _}
    val pkgName = newInteractiveExpressionPackage()

    val symbolValidator = exprDef.profile.symbolValidator

    val (code, offset) = expressionCode(exprDef, pkgName, noMacroProcessing = true)
    val sourceFile = new BatchSourceFile(pkgName, code)
    val pos = sourceFile.position(offset)
    logger.debug(s"Computing scope completion for $exprDef")

    val treeResponse = new Response[Tree]
    askLoadedTyped(sourceFile, treeResponse)
    val sourceTree = getOrThrow(treeResponse)
    val errors = compiler.reporter.compileErrors()

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

    val members = inCompilerThread {
      scope.collect {
        case member@ScopeMember(sym, _, _, viaImport)
          if viaImport != EmptyTree && sym.isTerm && !sym.isPackage &&
            (!isScexSynthetic(sym) || (isExpressionUtil(sym) && !isExpressionUtilObject(sym))) =>
          member
      } filter {
        m =>
          symbolValidator.validateMemberAccess(vc)(accessFromScopeMember(m)).deniedAccesses.isEmpty
      } map translateMember(vc)
    }

    removeUnitOf(sourceFile)

    Completion(members, errors)
  }

  protected def getTypeCompletion(exprDef: ExpressionDef) = withGlobal(exprDef.profile) { global =>
    import global.{sourceFile => _, position => _, _}
    val symbolValidator = exprDef.profile.symbolValidator

    val pkgName = newInteractiveExpressionPackage()

    val (code, offset) = expressionCode(exprDef, pkgName, noMacroProcessing = true)
    val sourceFile = new BatchSourceFile(pkgName, code)
    val startPosition = sourceFile.position(offset)

    logger.debug(s"Computing type completion for $exprDef")

    val treeResponse = new Response[Tree]
    askLoadedTyped(sourceFile, treeResponse)
    val fullTree = getOrThrow(treeResponse)

    val vc = ValidationContext(global)(getContextTpe(global)(fullTree))
    import vc._

    val members = inCompilerThread {
      // The compiler probably screws up tree positions when rewriting dynamic calls and because of that there's
      // literally no way to give the compiler a position under which it will find the appropriate tree.
      // As a result, I have to compute type completion manually instead of using [[askTypeCompletion]].
      // The code below is based on [[scala.tools.nsc.interactive.Global#typeMembers]].

      val tree = fullTree.collect {
        case ValDef(_, name, _, rhs) if name == newTermName("_result") => rhs
      }.lastOption.getOrElse(EmptyTree)

      val pre = stabilizedType(tree)

      val ownerTpe = tree.tpe match {
        case analyzer.ImportType(expr) => expr.tpe
        case null => pre
        case MethodType(List(), rtpe) => rtpe
        case _ => tree.tpe
      }

      val context = locateContext(startPosition).get

      val applicableViews: List[analyzer.SearchResult] =
        if (ownerTpe.isErroneous) Nil
        else new analyzer.ImplicitSearch(
          tree, definitions.functionType(List(ownerTpe), definitions.AnyClass.tpe), isView = true,
          context0 = context.makeImplicit(reportAmbiguousErrors = false)).allImplicits

      def viewApply(tree: Tree, view: analyzer.SearchResult): Tree = {
        assert(view.tree != EmptyTree)
        analyzer.newTyper(context.makeImplicit(reportAmbiguousErrors = false))
          .typed(Apply(view.tree, List(tree)).setPos(tree.pos))
          .onTypeError(EmptyTree)
      }

      def isMemberAllowed(qualifier: Tree, member: TermSymbol) =
        symbolValidator.validateMemberAccess(vc)(extractAccess(Select(qualifier, member))).deniedAccesses.isEmpty

      val fakeDirectPrefix = Ident(nme.EMPTY).setSymbol(Option(tree.symbol).getOrElse(NoSymbol)).setType(ownerTpe)

      val directMembers = ownerTpe.members.iterator.collect {
        case member: TermSymbol if member.isPublic && !member.isConstructor && isMemberAllowed(fakeDirectPrefix, member) =>
          val memberTpe = ownerTpe.memberType(member)
          translateMember(vc)(TypeMember(member, memberTpe, accessible = true, inherited = false, NoSymbol))
      }

      val implicitMembers = applicableViews.iterator.flatMap { view =>
        val convertedPrefix = viewApply(tree, view)
        val implicitTpe = stabilizedType(convertedPrefix)
        val fakeImplicitPrefix = Apply(view.tree, List(fakeDirectPrefix))
          .setSymbol(Option(convertedPrefix.symbol).getOrElse(NoSymbol)).setType(implicitTpe)

        implicitTpe.members.iterator.collect {
          case member: TermSymbol if member.isPublic && !member.isConstructor && !isFromToplevelType(member)
            && !isAdapterWrappedMember(member) && isMemberAllowed(fakeImplicitPrefix, member) =>
            val memberTpe = implicitTpe.memberType(member)
            translateMember(vc)(TypeMember(member, memberTpe, accessible = true, inherited = false, view.tree.symbol))
        }
      }

      (directMembers ++ implicitMembers).toList
    }

    removeUnitOf(sourceFile)

    Completion(members, Nil)
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
        typeCompletionCache.invalidateAll()
        init()
      }
    }
  }
}

object ScexPresentationCompiler {

  case class Param(name: String, tpe: String)

  case class Member(name: String, params: List[List[Param]], tpe: String, iimplicit: Boolean)

  case class Completion(members: List[Member], errors: List[CompileError])

}