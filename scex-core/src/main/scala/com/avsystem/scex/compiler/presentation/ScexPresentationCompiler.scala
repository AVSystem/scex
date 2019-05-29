package com.avsystem.scex
package compiler.presentation

import java.{lang => jl, util => ju}

import com.avsystem.commons.misc.TypeString
import com.avsystem.scex.compiler.CodeGeneration._
import com.avsystem.scex.compiler.ScexCompiler.{CompilationFailedException, CompileError}
import com.avsystem.scex.compiler.presentation.ScexPresentationCompiler.{Completion, MemberFlags, Param, Member => SMember}
import com.avsystem.scex.compiler.{ExpressionDef, _}
import com.avsystem.scex.parsing.EmptyPositionMapping
import com.avsystem.scex.presentation.annotation.{Documentation, ParameterNames}
import com.avsystem.scex.presentation.{Attributes, SymbolAttributes}
import com.avsystem.scex.util.CommonUtils._
import com.avsystem.scex.validation.ValidationContext
import com.avsystem.scex.{Type => SType}

import scala.collection.JavaConverters._
import scala.reflect.NameTransformer
import scala.tools.nsc.Settings

trait ScexPresentationCompiler extends ScexCompiler { compiler =>

  private val logger = createLogger[ScexPresentationCompiler]

  protected def isEnabled: Boolean = !settings.noPresentation.value
  protected def presentationCompilerSettings: Settings = settings

  private var reporter: Reporter = _
  private var global: IGlobal = _

  override protected def setup(): Unit = {
    super.setup()
    if (isEnabled) {
      logger.info("Initializing Scala presentation compiler")
      reporter = new Reporter(presentationCompilerSettings)
      global = new IGlobal(presentationCompilerSettings, reporter, getSharedClassLoader)
    }
  }

  def getOrThrow[T](resp: IGlobal#Response[T]) = resp.get match {
    case Left(res) => res
    case Right(t) => throw t
  }

  private def inCompilerThread[T](code: => T) = {
    getOrThrow(global.askForResponse(() => code))
  }

  protected final def withIGlobal[T](code: IGlobal => T) = underLock {
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
    resultType: String,
    variableTypes: Map[String, String]) {

    require(profile != null, "Profile cannot be null")
    require(contextType != null, "Context type cannot be null")
    require(rootObjectClass != null, "Root object class cannot be null")
    require(resultType != null, "Result type cannot be null")

    private def exprDef(expression: String, bare: Boolean) = {
      val (actualExpression, positionMapping) =
        if (bare) (expression, EmptyPositionMapping) else preprocess(expression, template)

      ExpressionDef(profile, template && !bare, setter && !bare, actualExpression, header, contextType,
        if (bare) "Any" else resultType, variableTypes)(expression, positionMapping, rootObjectClass)
    }

    // sometimes presentation compiler just fails to typecheck things and crashes
    // until we know what's happening it's better to return some default value (e.g. empty completion)
    // instead of crashing
    private def workaroundAssertionError[T](expr: => T, default: T): T =
      try expr catch {
        case e: AssertionError =>
          logger.error(s"Presentation compiler crashed, returning $default", e)
          default
      }

    def getErrors(expression: String): List[CompileError] =
      workaroundAssertionError(compiler.getErrors(exprDef(expression, bare = false)), Nil)

    def getErrorsAsJava(expression: String): ju.List[CompileError] =
      getErrors(expression).asJava

    def getScopeCompletion: Completion =
      workaroundAssertionError(compiler.getScopeCompletion(exprDef("()", bare = true)), Completion.Empty)

    def getTypeCompletion(expression: String, position: Int): Completion =
      workaroundAssertionError(compiler.getTypeCompletion(exprDef(expression, bare = false), position), Completion.Empty)

    def parse(expression: String): ast.Tree =
      compiler.parse(exprDef(expression, bare = false))
  }

  def getCompleter[C <: ExpressionContext[_, _], T](
    profile: ExpressionProfile,
    template: Boolean = true,
    setter: Boolean = false,
    variableTypes: Map[String, TypeString[_]] = Map.empty,
    header: String = ""
  )(implicit
    cti: ContextTypeInfo[C],
    tts: TypeString[T]
  ): Completer = {

    val strVariableTypes = variableTypes.iterator.map({ case (k, v) => (k, v.value) }).toMap
    val rootObjectClass = try cti.resolveRootClass() catch {
      case _: ClassNotFoundException => null
    }

    getCompleter(profile, template, setter, header, cti.fullTypeString, rootObjectClass, tts.value, strVariableTypes)
  }

  protected def getCompleter(
    profile: ExpressionProfile,
    template: Boolean,
    setter: Boolean,
    header: String,
    contextType: String,
    rootObjectClass: Class[_],
    resultType: String,
    variableTypes: Map[String, String]): Completer = {

    new Completer(profile, template, setter, header, contextType, rootObjectClass, resultType, variableTypes)
  }

  private def getContextTpe(global: IGlobal)(tree: global.Tree): global.Type = {
    import global._

    inCompilerThread {
      val PackageDef(_, List(ClassDef(_, _, _, Template(List(expressionParent, _), _, _)), _*)) = tree
      val TypeRef(_, _, List(contextTpe, _)) = expressionParent.tpe
      contextTpe
    }
  }

  private def getAttributes(global: IGlobal, attrs: SymbolAttributes)(member: global.ScexMember) = {

    import global._

    val m = member match {
      case tm: ScexTypeMember if tm.implicitlyAdded && isAdapter(tm.implicitType) =>
        tm.copy(sym = getJavaGetter(tm.sym, tm.ownerTpe), implicitTree = EmptyTree, implicitType = NoType)
      case _ => member
    }

    def merge(left: Stream[attrs.InfoWithIndex], right: Stream[attrs.InfoWithIndex]): Stream[attrs.InfoWithIndex] =
      (left, right) match {
        case (lh #:: lt, rh #:: rt) =>
          if (lh.index < rh.index)
            lh #:: merge(lt, right)
          else
            rh #:: merge(left, rt)
        case (_, Stream.Empty) =>
          left
        case (Stream.Empty, _) =>
          right
      }

    def implicitConv =
      if (m.implicitlyAdded) Some(stripTypeApply(m.implicitTree)) else None

    def normalInfos =
      attrs.matchingInfos(global)(m.ownerTpe, m.sym, implicitConv)

    def implicitInfos =
      if (m.implicitlyAdded)
        attrs.matchingInfos(global)(m.implicitType, m.sym, None)
      else Nil

    def attributesFromInfos =
      merge(normalInfos.toStream, implicitInfos.toStream).map(_.info.payload)

    def annotValue(annotTree: Tree) = annotTree.children.tail.collectFirst {
      case AssignOrNamedArg(Ident(TermName("value")), value) => value
      case _ => None
    }

    def parseAnnotation(ann: Annotation): Attributes = {
      if (ann.tree.tpe <:< typeOf[ParameterNames]) {
        val paramNames = annotValue(ann.tree).map {
          case Apply(_, paramNameLiterals) => paramNameLiterals.map {
            case LiteralString(name) => name
          }
        }
        new Attributes(paramNames, None)
      } else if (ann.tree.tpe <:< typeOf[Documentation]) {
        val documentation = annotValue(ann.tree).map {
          case LiteralString(doc) => doc
        }
        new Attributes(None, documentation)
      } else Attributes.empty
    }

    def attributesFromAnnotations =
      withOverrides(m.sym).iterator.flatMap(s => annotations(s)).map(parseAnnotation).toStream

    def foldAttributes(str: Stream[Attributes]): Attributes = str match {
      case head #:: tail => head orElse foldAttributes(tail)
      case Stream.Empty => Attributes.empty
    }

    foldAttributes(attributesFromInfos append attributesFromAnnotations)
  }

  private def translateMember(global: IGlobal, attrs: SymbolAttributes)(member: global.ScexMember) = {
    import global._

    def translateType(tpe: Type) =
      if (tpe == NoType) SType.NoType
      else tpe.toOpt.map { tpe =>
        SType(tpe.map(_.dealiasWiden).toString(), erasureClass(tpe))
      }.getOrElse(SType.NoType)

    val attributes = getAttributes(global, attrs)(member)
    val (params, implParams) = paramsOf(member.tpe)
    val nameOverrides = (params.flatten zip attributes.paramNames.getOrElse(Nil)).toMap

    def symbolToParam(sym: Symbol) =
      Param(nameOverrides.getOrElse(sym, sym.decodedName), translateType(sym.typeSignature))

    val adapter = isAdapter(member.sym.owner.toType)
    lazy val adaptedType =
      if (member.implicitlyAdded) member.ownerTpe
      else member.ownerTpe.declaration(TermName(CodeGeneration.AdapterWrappedSymbol)).typeSignatureIn(member.ownerTpe)
    val javaMember = memberToJava(if (adapter) getJavaGetter(member.sym, adaptedType) else member.sym)
    val rootMember = isAnnotatedWith(member.ownerTpe.widen, rootValueAnnotType)

    SMember(member.sym.decodedName,
      params.map(_.map(symbolToParam)),
      implParams.map(symbolToParam),
      translateType(member.ownerTpe),
      translateType(member.tpe.finalResultType),
      MemberFlags(member.sym.isImplicit, adapter, rootMember),
      javaMember,
      attributes.documentation)
  }

  protected def getErrors(exprDef: ExpressionDef) = withIGlobal { global =>
    val (pkgName, code, offset) = expressionCode(exprDef)
    val sourceFile = new ExpressionSourceFile(exprDef, pkgName, code, offset)

    val response = new global.Response[global.Tree]
    try {
      global.askLoadedTyped(sourceFile, keepLoaded = true, response)
      getOrThrow(response)
      reporter.compileErrors()
    } finally {
      val resp = new global.Response[Unit]
      global.askFilesDeleted(List(sourceFile), resp)
      getOrThrow(resp)
    }
  }

  protected def getScopeCompletion(exprDef: ExpressionDef): Completion = withIGlobal { global =>
    val symbolValidator = exprDef.profile.symbolValidator
    val symbolAttributes = exprDef.profile.symbolAttributes

    val (pkgName, code, offset) = expressionCode(exprDef, noMacroProcessing = true)
    val sourceFile = new ExpressionSourceFile(exprDef, pkgName, code, offset)

    import global.{position => _, sourceFile => _, _}
    try {
      val pos = sourceFile.position(offset)
      logger.debug(s"Computing scope completion for $exprDef")

      val treeResponse = new Response[Tree]
      askLoadedTyped(sourceFile, keepLoaded = true, treeResponse)
      val sourceTree = getOrThrow(treeResponse)

      val vc = ValidationContext(global)(getContextTpe(global)(sourceTree))
      import vc._

      def accessFromScopeMember(m: ScexScopeMember): MemberAccess =
        extractAccess(Select(m.viaImport, m.sym))

      inCompilerThread {
        val scope: Vector[ScexScopeMember] = scopeMembers(pos)
        val membersIterator = scope.iterator.filter { m =>
          m.viaImport != EmptyTree && m.sym.isTerm && !m.sym.hasPackageFlag && !isFromProfileObject(m.sym) &&
            symbolValidator.validateMemberAccess(vc)(accessFromScopeMember(m)).deniedAccesses.isEmpty
        } map translateMember(global, symbolAttributes)

        Completion(ast.EmptyTree, membersIterator.toVector)
      }

    } finally {
      val resp = new global.Response[Unit]
      global.askFilesDeleted(List(sourceFile), resp)
      getOrThrow(resp)
    }
  }

  protected def getTypeCompletion(exprDef: ExpressionDef, position: Int): Completion = withIGlobal { global =>
    logger.debug(s"Computing type completion for $exprDef at position $position")
    val startTime = System.nanoTime()

    val symbolValidator = exprDef.profile.symbolValidator
    val symbolAttributes = exprDef.profile.symbolAttributes

    val (pkgName, code, offset) = expressionCode(exprDef, noMacroProcessing = true)
    val sourceFile = new ExpressionSourceFile(exprDef, pkgName, code, offset)

    import global.{position => _, sourceFile => _, _}

    val positionFixer: Traverser = new Traverser {
      override def traverse(tree: Tree) = {
        super.traverse(tree)
        tree match {
          // treat keywords incorrectly typed after dot as part of the Select tree
          case Select(_, termNames.ERROR) =>
            val chars = sourceFile.content
            val newEnd = tree.pos.end + Iterator.range(tree.pos.end, chars.length)
              .takeWhile(i => chars(i).isLetter).length
            tree.setPos(tree.pos.withEnd(newEnd))
          // fix selectDynamic positions, which scalac computes incorrectly...
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

    val result = {
      try {
        val sourcePosition = sourceFile.position(offset + exprDef.positionMapping(position))

        val treeResponse = new Response[Tree]
        askLoadedTyped(sourceFile, keepLoaded = true, treeResponse)
        val fullTree = getOrThrow(treeResponse)

        val vc = ValidationContext(global)(getContextTpe(global)(fullTree))
        import vc._

        inCompilerThread {
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

          val completionCtx = global.typeCompletionContext(tree, sourcePosition)
          logger.debug("Prefix tree for type completion:\n" + show(completionCtx.prefixTree, printTypes = true, printPositions = true))

          val members = getTypeMembers(global)(exprDef, completionCtx.ownerTpe) {
            val typeMembers = global.typeMembers(completionCtx)

            val fakeDirectPrefix = fakeIdent(completionCtx.ownerTpe, tree.symbol)

            def fakeSelect(member: ScexTypeMember) = {
              val fakePrefix =
                if (!member.implicitlyAdded) fakeDirectPrefix
                else Apply(member.implicitTree, List(fakeDirectPrefix))
                  .setSymbol(member.implicitTree.symbol).setType(member.implicitType)
              Select(fakePrefix, member.sym)
            }

            typeMembers.collect {
              case m if m.sym.isTerm && m.sym.isPublic && !m.sym.isConstructor
                && !isAdapterWrappedMember(m.sym) && isAllowed(fakeSelect(m)) => translateMember(global, symbolAttributes)(m)
            }
          }

          val translator = new ast.Translator(global, offset, exprDef)
          val translatedTree = translator.translateTree(completionCtx.prefixTree.asInstanceOf[translator.u.Tree])

          Completion(translatedTree, members)
        }

      } finally {
        val resp = new global.Response[Unit]
        global.askFilesDeleted(List(sourceFile), resp)
        getOrThrow(resp)
      }
    }

    val duration = System.nanoTime() - startTime
    logger.debug(s"Completion took ${duration / 1000000}ms")

    result
  }

  // method extracted in order to make it possible to cache results by some other trait
  protected def getTypeMembers(global: IGlobal)(exprDef: ExpressionDef, ownerTpe: global.Type)
    (computeMembers: => Vector[SMember]): Vector[SMember] = computeMembers

  protected def parse(exprDef: ExpressionDef) = withIGlobal { global =>
    inCompilerThread {
      val parsedTree = global.parseExpression(exprDef.expression, exprDef.template)
      val translator = new ast.Translator(global, 0, exprDef)
      translator.translateTree(parsedTree.asInstanceOf[translator.u.Tree])
    }
  }

  @throws[CompilationFailedException]
  def compileSymbolAttributes(source: NamedSource): SymbolAttributes = underLock {
    val pkgName = SymbolAttributesPkgPrefix + NameTransformer.encode(source.name)
    val codeToCompile = wrapInSource(generateSymbolAttributes(source.code), pkgName)
    val sourceFile = new ScexSourceFile(pkgName, codeToCompile, shared = true)

    compile(sourceFile) match {
      case Left(classLoader) =>
        instantiate[SymbolAttributes](classLoader, s"$pkgName.$SymbolAttributesClassName")
      case Right(errors) =>
        throw new CompilationFailedException(codeToCompile, errors)
    }
  }

  override protected def compile(sourceFile: ScexSourceFile) = {
    val result = super.compile(sourceFile)

    if (isEnabled) {
      result match {
        case Left(_) if sourceFile.shared =>
          val global = this.global
          val response = new global.Response[global.Tree]
          global.askLoadedTyped(sourceFile, keepLoaded = true, response)
          getOrThrow(response)
        case _ =>
      }
    }

    result
  }

  override def reset(): Unit =
    if (isEnabled) {
      underLock {
        global.askShutdown()
        super.reset()
      }
    } else super.reset()
}

object ScexPresentationCompiler {

  case class Param(name: String, tpe: Type)

  case class MemberFlags(iimplicit: Boolean, javaGetterAdapter: Boolean, inputMember: Boolean)

  case class Member(name: String, params: List[List[Param]], implicitParams: List[Param], ownerType: Type, returnType: Type,
    flags: MemberFlags, javaMember: Option[jl.reflect.Member], documentation: Option[String]) {

    def paramsAsJava =
      params.map(_.asJava).asJava

    def implicitParamsAsJava =
      implicitParams.asJava

    def javaField = javaMember.filterByClass[jl.reflect.Field]

    def javaMethod = javaMember.filterByClass[jl.reflect.Method]

    def javaConstructor = javaMember.filterByClass[jl.reflect.Constructor[_]]
  }

  case class Completion(typedPrefixTree: ast.Tree, members: Vector[Member]) {
    def membersAsJava = members.asJava

    def withMembers(newMembers: ju.Collection[Member]) =
      copy(members = newMembers.asScala.toVector)
  }
  object Completion {
    final val Empty = Completion(ast.EmptyTree, Vector.empty)
  }

}
