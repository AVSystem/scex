package com.avsystem.scex
package compiler.presentation

import com.avsystem.scex.compiler._
import com.avsystem.scex.util.MacroUtils
import com.avsystem.scex.validation.ValidationContext
import com.google.common.cache.CacheBuilder
import java.{util => ju, lang => jl}
import scala.reflect.internal.util.{SourceFile, BatchSourceFile}
import scala.reflect.runtime.universe.TypeTag
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

  private def withGlobal[T](code: IGlobal => T) = underLock {
    reporter.reset()
    val global = compiler.global
    val result = try code(global) finally {
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

    private def exprDef(expression: String, bare: Boolean) =
      ExpressionDef(profile, template && !bare, setter && !bare, expression, header,
        rootObjectClass, contextType, if (bare) "Any" else resultType)

    def getErrors(expression: String): List[CompileError] =
      compiler.getErrors(exprDef(expression, bare = false))

    def getScopeCompletion: Completion =
      compiler.getScopeCompletion(exprDef("()", bare = true))

    def getTypeCompletion(expression: String, position: Int): Completion =
      getPrefixForCompletion(expression, template, position) match {
        case Some(subexpr) => compiler.getTypeCompletion(exprDef(subexpr, bare = true))
        case None => Completion(Nil)
      }

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

  protected def getPrefixForCompletion(expression: String, template: Boolean, position: Int): Option[String] = {
    val global = compiler.global
    import global.{sourceFile => _, position => _, _}

    val (code, offset) = CodeGeneration.wrapForParsing(expression, template)
    val sourceFile = new BatchSourceFile("(for_parsing)", code)
    val sourcePosition = sourceFile.position(offset + position)
    val PackageDef(_, List(ModuleDef(_, _, Template(_, _, List(_, expressionTree))))) = parseTree(sourceFile)

    def includes(p1: Position, p2: Position) =
      p1.startOrPoint <= p2.point && p1.endOrPoint > p2.point

    if (position >= 0 && expression.length > position && expression.charAt(position) == '.')
      new Locator(sourcePosition).locateIn(expressionTree) match {
        case tree@Literal(Constant(str: String)) if includes(tree.pos, sourcePosition) =>
          None
        case tree@Ident(_) if includes(tree.pos, sourcePosition) =>
          None
        case tree@Select(qual, name) if includes(tree.pos, sourcePosition) && !includes(qual.pos, sourcePosition) =>
          None
        case tree@Literal(Constant(dbl: Double)) if dbl.isWhole && tree.pos.end - 1 == sourcePosition.point =>
          Some(dbl.toLong.toString)
        case tree =>
          Some(tree.toString())
      }
    else None
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

      val members = inCompilerThread {
        scope.collect {
          case member@ScopeMember(sym, _, _, viaImport)
            if viaImport != EmptyTree && sym.isTerm && !sym.isPackage &&
              !isAdapterWrappedMember(sym) && (!isScexSynthetic(sym) || (isExpressionUtil(sym) && !isExpressionUtilObject(sym))) =>
            member
        } filter { m =>
          symbolValidator.validateMemberAccess(vc)(accessFromScopeMember(m)).deniedAccesses.isEmpty
        } map translateMember(vc)
      }

      Completion(members)

    } finally {
      removeUnitOf(sourceFile)
    }
  }

  protected def getTypeCompletion(exprDef: ExpressionDef) = withGlobal { global =>
    import global.{sourceFile => _, position => _, _}
    val symbolValidator = exprDef.profile.symbolValidator

    val pkgName = newInteractiveExpressionPackage()

    val (code, offset) = expressionCode(exprDef, pkgName, noMacroProcessing = true)
    val sourceFile = new ExpressionSourceFile(exprDef.profile, pkgName, code)

    try {
      val startPosition = sourceFile.position(offset)

      logger.debug(s"Computing type completion for $exprDef")

      val treeResponse = new Response[Tree]
      askLoadedTyped(sourceFile, treeResponse)
      val fullTree = getOrThrow(treeResponse)

      val vc = ValidationContext(global)(getContextTpe(global)(fullTree))
      import vc._

      val members = inCompilerThread {
        val tree = fullTree.collect {
          case ValDef(_, name, _, rhs) if name == newTermName("_result") => rhs
        }.lastOption.getOrElse(EmptyTree)

        val (ownerTpe, typeMembers) = global.typeMembers(tree, startPosition)

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

        typeMembers.collect {
          case m if m.sym.isTerm && m.sym.isPublic && !m.sym.isConstructor
            && !isAdapterWrappedMember(m.sym) && isMemberAllowed(m) => translateMember(vc)(m)
        }
      }

      Completion(members)

    } finally {
      removeUnitOf(sourceFile)
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
        typeCompletionCache.invalidateAll()
        init()
      }
    }
  }
}

object ScexPresentationCompiler {

  case class Param(name: String, tpe: String)

  case class Member(name: String, params: List[List[Param]], tpe: String, iimplicit: Boolean)

  case class Completion(members: List[Member])

}