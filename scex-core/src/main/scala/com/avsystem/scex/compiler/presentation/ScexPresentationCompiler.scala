package com.avsystem.scex
package compiler.presentation

import com.google.common.cache.CacheBuilder
import compiler.ScexCompiler.CompileError
import compiler.{ExpressionDef, ExpressionMacroProcessor, ScexCompiler}
import java.{util => ju, lang => jl}
import scala.reflect.internal.util.{SourceFile, BatchSourceFile}
import scala.reflect.runtime.universe.TypeTag
import scala.tools.nsc.interactive.{Global => IGlobal}
import util.CommonUtils._
import com.avsystem.scex.validation.{FakeImplicitConversion, ValidationContext}
import com.avsystem.scex.validation.SymbolValidator.MemberAccessSpec

trait ScexPresentationCompiler extends ScexCompiler {
  compiler =>

  import ScexPresentationCompiler.{Member => SMember, Param, Completion}

  private val logger = createLogger[ScexPresentationCompiler]

  private object lock

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

    def getScopeCompletion(expression: String, position: Int): Completion

    def getTypeCompletion(expression: String, position: Int): Completion
  }

  private def inCompilerThread[T](code: => T) = {
    getOrThrow(global.askForResponse(() => code))
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

    val pkgName = newInteractiveExpressionPackage()

    private def withGlobal[T](code: IGlobal => T) = underLock {
      val global = compiler.global
      inCompilerThread {
        ExpressionMacroProcessor.profileVar.value = profile
      }
      val result = code(global)
      inCompilerThread {
        ExpressionMacroProcessor.profileVar.value = null
      }
      reporter.reset()
      result
    }

    private def getContextTpe(global: IGlobal)(tree: global.Tree): global.Type = {
      import global._

      inCompilerThread {
        val PackageDef(_, List(ClassDef(_, _, _, Template(List(_, expressionParent, _), _, _)))) = tree
        val TypeRef(_, _, List(contextTpe, _)) = expressionParent.tpe
        contextTpe
      }
    }

    private def translateMember(vc: ValidationContext {val universe: IGlobal})(member: vc.universe.Member) = {
      import vc._
      import vc.universe._

      def symbolToParam(sym: Symbol) =
        Param(sym.decodedName, sym.typeSignature.toString())

      SMember(member.sym.decodedName,
        paramsOf(member.tpe).map(_.map(symbolToParam)),
        resultTypeOf(member.tpe).toString(),
        member.sym.isImplicit)
    }

    def getErrors(expression: String) = withGlobal { global =>
      val exprDef = ExpressionDef(profile, template, setter, expression, header, rootObjectClass, contextType, resultType)
      val (code, _) = expressionCode(exprDef, pkgName)
      val response = new global.Response[global.Tree]
      global.askLoadedTyped(new BatchSourceFile(pkgName, code), response)
      getOrThrow(response)
      reporter.compileErrors()
    }

    def getScopeCompletion(expression: String, position: Int): Completion = withGlobal { global =>

      import global.{sourceFile => _, position => _, _}
      val symbolValidator = profile.symbolValidator

      val exprDef = ExpressionDef(profile, template, setter, expression, header, rootObjectClass, contextType, resultType)

      val (code, offset) = expressionCode(exprDef, pkgName)
      val sourceFile = new BatchSourceFile(pkgName, code)
      val pos = sourceFile.position(offset + position)
      logger.debug(s"Computing scope completion for $exprDef at position $position\n${pos.lineContent}\n${" " * (pos.column - 1)}^")

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

      val deleteResponse = new Response[Unit]
      askFilesDeleted(List(sourceFile), deleteResponse)
      getOrThrow(deleteResponse)

      Completion(members, errors)
    }

    def getTypeCompletion(expression: String, position: Int): Completion = withGlobal { global =>
      import global.{sourceFile => _, position => _, _}
      val symbolValidator = profile.symbolValidator

      val exprDef = ExpressionDef(profile, template, setter, expression, header, rootObjectClass, contextType, resultType)
      val (code, offset) = expressionCode(exprDef, pkgName)
      val sourceFile = new BatchSourceFile(pkgName, code)

      val actualPosition = offset + position
      val pos = global.rangePos(sourceFile, actualPosition, actualPosition, actualPosition)

      logger.debug(s"Computing type completion for $exprDef at position $position\n${pos.lineContent}\n${" " * (pos.column - 1)}^")

      val treeResponse = new Response[Tree]
      askLoadedTyped(sourceFile, treeResponse)
      val sourceTree = getOrThrow(treeResponse)
      val errors = compiler.reporter.compileErrors()

      val vc = ValidationContext(global)(getContextTpe(global)(sourceTree))
      import vc._

      val typedTreeResponse = new Response[Tree]
      askTypeAt(pos, typedTreeResponse)
      val typedTree = getOrThrow(typedTreeResponse)

      //various finetunings of how the presentation compiler works

      def normalizedTreeType(tree: Tree): Type = tree match {
        case _ if tree.symbol != null && tree.symbol.isPackage =>
          tree.tpe.member(nme.PACKAGE) match {
            case sym: TermSymbol => normalizeType(tree.tpe.memberType(sym))
            case _ => tree.tpe
          }
        case ValDef(_, _, _, rhs) => normalizedTreeType(rhs)
        case Select(prefix, nme.ERROR) => normalizedTreeType(prefix)
        case Select(prefix, name: TermName) if (tree.tpe == NoType || tree.tpe == ErrorType) && prefix.tpe <:< typeOf[Dynamic] =>
          normalizeType(prefix.tpe.memberType(prefix.tpe.member(newTermName("selectDynamic"))))
        case _ => normalizeType(tree.tpe)
      }

      def normalizeType(tpe: Type): Type = tpe match {
        case null => NoType
        case TypeRef(_, _, _) |
             ConstantType(_) |
             SingleType(_, _) |
             RefinedType(_, _) |
             ExistentialType(_, _) |
             ThisType(_) |
             SuperType(_, _) |
             WildcardType |
             BoundedWildcardType(_) =>
          tpe
        case MethodType(_, resultTpe) =>
          normalizeType(resultTpe)
        case NullaryMethodType(resultTpe) =>
          normalizeType(resultTpe)
        case AnnotatedType(_, underlying, _) =>
          normalizeType(underlying)
        case _ =>
          NoType
      }

      val members = inCompilerThread {
        val tpe = normalizedTreeType(typedTree)
        val cacheKey = (this, TypeWrapper(global)(tpe))

        logger.debug(s"Type is $tpe on tree ${showRaw(typedTree)}")

        typeCompletionCache.get(cacheKey, callable {

          // type completion is generated based on members allowed in SymbolValidator ACL
          symbolValidator.accessSpecs.flatMap {
            case MemberAccessSpec(typeInfo, signature, implicitConv, true) if tpe <:< typeInfo.typeIn(global) =>
              val treeSymbol = if (typedTree.symbol != null) typedTree.symbol else NoSymbol
              val barePrefix = Ident(nme.EMPTY).setType(tpe).setSymbol(treeSymbol).setPos(typedTree.pos)

              /* Implicit conversion handling here is a hack, we don't have full information to perform
              full validation and compute exact return type for a member available by implicit view.
              In fact, we don't even know if an appriopriate implicit conversion is in scope.
              We don't want to use 'askTypeCompletion' because of previously observed ridiculously
              nondeterministic behaviour of the presentation compiler when doing so */

              val prefix = implicitConv match {
                case Some((implicitPath, implicitTypeInfo)) =>
                  val fakeConversion = Ident(nme.EMPTY).updateAttachment(FakeImplicitConversion(implicitPath))
                  Apply(fakeConversion, List(barePrefix)).setType(implicitTypeInfo.typeIn(global))
                case None => barePrefix
              }

              val symbol = memberBySignature(prefix.tpe, signature)
              if (symbol != NoSymbol && !symbol.isConstructor) {
                val memberTpe = prefix.tpe.memberType(symbol)
                val accessTree = Select(prefix, symbol.name).setSymbol(symbol).setType(memberTpe)

                val allowed = symbolValidator.validateMemberAccess(vc)(extractAccess(accessTree)).deniedAccesses.isEmpty
                if (allowed)
                  Some(translateMember(vc)(TypeMember(symbol, memberTpe, accessible = true, inherited = false, NoSymbol)))
                else None

              } else None

            case _ => None
          }
        })

      }

      val deleteResponse = new Response[Unit]
      askFilesDeleted(List(sourceFile), deleteResponse)
      getOrThrow(deleteResponse)

      Completion(members, errors)
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