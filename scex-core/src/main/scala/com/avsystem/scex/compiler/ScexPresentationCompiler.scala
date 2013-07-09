package com.avsystem.scex.compiler

import com.avsystem.scex.validation.{ValidationContext, ExpressionValidator}
import java.{util => ju, lang => jl}
import scala.reflect.internal.util.{SourceFile, BatchSourceFile}
import scala.tools.nsc.interactive.{Global => IGlobal}
import com.avsystem.scex.compiler.ScexCompiler.CompileError

trait ScexPresentationCompiler extends ScexCompiler {
  compiler =>

  import ScexPresentationCompiler.{Member => SMember, Param, Completion}

  private val reporter = new Reporter(settings)
  private var global: IGlobal = _

  private def init() {
    global = new IGlobal(settings, reporter)
  }

  init()

  private def newInteractiveExpressionPackage() =
    newPackageName("_scex_interactive_expr")

  def getOrThrow[T](resp: IGlobal#Response[T]) = resp.get match {
    case Left(res) => res
    case Right(t) => throw t
  }

  class InteractiveContext(
    profile: ExpressionProfile,
    contextType: String,
    contextClass: Class[_],
    resultType: String) {

    val pkgName = newInteractiveExpressionPackage()

    private def withGlobal[T](code: IGlobal => T) = compiler.synchronized {
      val global = compiler.global
      getOrThrow(global.askForResponse(() => ExpressionValidator.profileVar.value = profile))
      val result = code(global)
      getOrThrow(global.askForResponse(() => ExpressionValidator.profileVar.value = null))
      result
    }

    private def getContextTpe(global: IGlobal)(tree: global.Tree): global.Type = {
      import global._

      getOrThrow(askForResponse { () =>
        val PackageDef(_, List(ClassDef(_, _, _, Template(List(_, parent), _, _)))) = tree
        val TypeRef(_, _, List(contextTpe, _)) = parent.tpe
        contextTpe
      })
    }

    private def translateMember(vc: ValidationContext {val universe: IGlobal})(member: vc.universe.Member) = {
      import vc._
      import vc.universe._

      def symbolToParam(sym: Symbol) =
        Param(sym.decodedName, sym.typeSignature.toString())

      SMember(member.sym.decodedName, paramsOf(member.tpe).map(_.map(symbolToParam)), resultTypeOf(member.tpe).toString())
    }

    def getErrors(expression: String) = withGlobal { global =>
      val exprDef = ExpressionDef(profile, expression, contextClass, contextType, resultType)
      val (code, _) = expressionCode(exprDef, pkgName)
      val response = new global.Response[global.Tree]
      global.askLoadedTyped(new BatchSourceFile(pkgName, code), response)
      getOrThrow(response)
      reporter.compileErrors()
    }

    def getScopeCompletion(expression: String, position: Int): Completion = withGlobal { global =>
      import global.{sourceFile => _, position => _, _}
      val symbolValidator = profile.symbolValidator

      val exprDef = ExpressionDef(profile, expression, contextClass, contextType, resultType)
      val (code, offset) = expressionCode(exprDef, pkgName)
      val sourceFile = new BatchSourceFile(pkgName, code)

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
      askScopeCompletion(sourceFile.position(offset + position), response)
      val scope = getOrThrow(response)

      val members = getOrThrow(askForResponse { () =>
        scope.collect {
          case member@ScopeMember(sym, _, true, viaImport)
            if viaImport != EmptyTree && sym.isTerm && !sym.isPackage &&
              (!isScexSynthetic(sym) || (isExpressionUtil(sym) && !isExpressionUtilObject(sym))) =>
            member
        } filter { m =>
          symbolValidator.validateMemberAccess(vc)(accessFromScopeMember(m)).deniedAccesses.isEmpty
        } map translateMember(vc)
      })

      val deleteResponse = new Response[Unit]
      askFilesDeleted(List(sourceFile), deleteResponse)
      getOrThrow(deleteResponse)

      Completion(members, errors)
    }

    def getTypeCompletion(expression: String, position: Int): Completion = withGlobal { global =>
      import global.{sourceFile => _, position => _, _}
      val symbolValidator = profile.symbolValidator

      val exprDef = ExpressionDef(profile, expression, contextClass, contextType, resultType)
      val (code, offset) = expressionCode(exprDef, pkgName)
      val sourceFile = new BatchSourceFile(pkgName, code)
      val pos = sourceFile.position(offset + position)

      val treeResponse = new Response[Tree]
      askLoadedTyped(sourceFile, treeResponse)
      val sourceTree = getOrThrow(treeResponse)
      val errors = compiler.reporter.compileErrors()

      val vc = ValidationContext(global)(getContextTpe(global)(sourceTree))
      import vc._

      val response = new Response[List[Member]]
      askTypeCompletion(pos, response)
      val scope = getOrThrow(response)

      val tree = locateTree(pos)
      val context = doLocateContext(pos)

      // Implicit conversions (conversion tree and implicit type) must be obtained manually from the
      // compiler because TypeMember contains only the implicit conversion symbol.
      // This code is heavily based on [[scala.tools.nsc.interactive.Global#typeMembers]]
      val implicitConversions: Map[Symbol, (Tree, Type)] =
        getOrThrow(askForResponse { () =>
          val applicableViews: List[analyzer.SearchResult] =
            new analyzer.ImplicitSearch(
              tree, definitions.functionType(List(tree.tpe), definitions.AnyClass.tpe), isView = true,
              context.makeImplicit(reportAmbiguousErrors = false), NoPosition).allImplicits

          applicableViews.map { searchResult =>
            val implicitTpe = analyzer.newTyper(context.makeImplicit(reportAmbiguousErrors = false))
              .typed(Apply(searchResult.tree, List(tree)) setPos tree.pos)
              .onTypeError(EmptyTree).tpe

            (searchResult.tree.symbol, (searchResult.tree, implicitTpe))
          }.toMap
        })

      def accessFromTypeMember(member: TypeMember) =
        if (!member.implicitlyAdded)
          extractAccess(Select(tree, member.sym))
        else {
          val (implicitTree, implicitTpe) = implicitConversions(member.viaView)
          extractAccess(Select(Apply(implicitTree, List(tree)).setSymbol(member.viaView).setType(implicitTpe), member.sym))
        }

      val members = getOrThrow(askForResponse { () =>
        scope.collect {
          case member: TypeMember
            if member.sym.isTerm && !member.sym.isConstructor && !isWrappedInAdapter(member.sym) =>
            member
        } filter { member =>
          symbolValidator.validateMemberAccess(vc)(accessFromTypeMember(member)).deniedAccesses.isEmpty
        } map translateMember(vc)
      })

      val deleteResponse = new Response[Unit]
      askFilesDeleted(List(sourceFile), deleteResponse)
      getOrThrow(deleteResponse)

      Completion(members, errors)
    }
  }

  def getInteractiveContext(profile: ExpressionProfile,
    contextType: String,
    contextClass: Class[_],
    resultType: String): InteractiveContext = {

    require(profile != null, "Profile cannot be null")
    require(contextType != null, "Context type cannot be null")
    require(contextClass != null, "Context class cannot be null")
    require(resultType != null, "Result type cannot be null")

    new InteractiveContext(profile, contextType, contextClass, resultType)
  }

  override protected def compile(sourceFile: SourceFile, classLoader: ScexClassLoader, usedInExpressions: Boolean) = {
    val result = super.compile(sourceFile, classLoader, usedInExpressions)

    if (result.isEmpty && usedInExpressions) {
      val global = this.global
      val response = new global.Response[global.Tree]
      global.askLoadedTyped(sourceFile, response)
      getOrThrow(response)
    }

    result
  }

  override def reset() {
    synchronized {
      super.reset()
      init()
    }
  }
}

object ScexPresentationCompiler {

  case class Param(name: String, tpe: String)

  case class Member(name: String, params: List[List[Param]], tpe: String)

  case class Completion(members: List[Member], errors: List[CompileError])

}