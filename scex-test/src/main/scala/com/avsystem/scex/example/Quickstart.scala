package com.avsystem.scex.example

import com.avsystem.scex.compiler.{DefaultScexCompiler, ScexSettings}
import com.avsystem.scex.presentation.SymbolAttributes
import com.avsystem.scex.util.{CommonSymbolValidators, PredefinedAccessSpecs, SimpleContext}
import com.avsystem.scex.validation.SymbolValidator.MemberAccessSpec
import com.avsystem.scex.validation.{SymbolValidator, SyntaxValidator}
import com.avsystem.scex.{Expression, ExpressionProfile, NamedSource}

object Quickstart {
  // First, we need to configure the Scala compiler used internally by Scex. `ScexSettings` extends
  // `scala.tools.nsc.Settings` which is the class that represents all the standard Scala compiler settings which
  // can be passed through command line. `ScexSettings` adds some additional, Scex-specific configuration, e.g.
  // for caching compiled expressions.
  //
  // Perhaps the most important thing to configure is the classpath. Scex compiler compiles expressions against
  // an actual classpath (it doesn't use runtime reflection) which needs to be properly configured. By default, it
  // will fetch classpath from `java.class.path` property of the JVM process that it runs in.
  val settings = new ScexSettings

  // Now we can create the compiler. `ScexCompiler` encapsulates an instance of actual Scala compiler. This means
  // that this is a very heavyweight object which should be reused whenever possible.
  val compiler = new DefaultScexCompiler(settings)

  // The next thing we need is an expression profile. An expression profile is an "environment" against which
  // expressions will be compiled. There may be multiple expression profiles used with a single compiler.
  // An expression profile consists of:
  // * Name - should uniquely identify the profile, it is used as part of cache keys in expression caches
  // * Syntax validator - a strategy which limits Scala syntactic constructs which can be used by an expression.
  //   Typically, `SyntaxValidator.SimpleExpressions` is used which actually limits the language only to its
  //   "expression" subset by disallowing all sorts of definitions and declarations (variables, methods, classes, etc.)
  // * Symbol validator - a strategy which limits API that can be used by an expression. A symbol validator MUST
  //   explicitly whitelist all the allowed methods. This means that you will have to maintain a symbol validator
  //   for your custom expression API. There is a Scala DSL for defining symbol validators which will be shown later.
  // * Symbol attributes - object containing additional metadata about expression API (i.e. method documentation).
  //   This is used by Scex presentation compiler which is a backend for implementing IDE-like features like API
  //   completion.
  // * Expression header - fragment of Scala code which will be inserted as-is into every expression.
  //   Header normally contains additional imports that we want to be injected into compilation of expressions.
  // * Expression utils - this is meant to be used only in pure Java projects using Scex. It's a Scala source which
  //   will be compiled and imported into every expression. Scala project should always use empty expression profile
  //   because they can simply implement "utils" as part of the project and import them through expression header.
  // * Dynamic variables flag - if set, enables dynamic access via the DynamicVariableAccessor interface to additional
  //   variables of the ExpressionContext#V type. If disabled, expression can access only typed additional variables
  //   carried through the expression context. This flag does not affect visibility of variables provided by the root
  //   object.
  val profile = new ExpressionProfile(
    "default",
    SyntaxValidator.SimpleExpressions,
    SymbolValidator(PredefinedAccessSpecs.basicOperations ++ CommonSymbolValidators.commonExpressionApi ++ rootAcl),
    SymbolAttributes.empty,
    "import com.avsystem.scex.util.CommonExpressionUtils._",
    NamedSource("empty", ""),
    dynamicVariablesEnabled = true,
  )

  // The root object of an expression. It defines the primary API exposed into expressions (apart from "static" utils
  // imported through expression header). Methods of the root object will be directly available inside the expression.
  trait Root {
    def someString: String
    def someInt: Int
  }

  // Access Control List which allows usage of Root API inside expressions.
  def rootAcl: List[MemberAccessSpec] = {
    import SymbolValidator._
    allow {
      on { r: Root =>
        // Note: this statically lists all methods of Root which means it needs to be recompiled each time Root changes.
        // Unfortunately such changes are usually not caught by the incremental compiler.
        r.all.introduced.members
      }
    }
  }

  // Now we can actually define and *compile* an expression. To do that, we need at least:
  // * The expression profile which we have just defined
  // * Type of an *expression context* (which encapsulates the type of root object) - this is specified simply as a
  //   type argument to `getCompiledExpression`. There's a macro-materialized typeclass (`ContextTypeInfo`) which
  //   carries this type information into runtime.
  // * Expected *result type* of an expression. This type is also specified as type argument and reified into runtime
  //   through macro-materialized `TypeString` typeclass.
  // * The expression itself. By default, it is compiled in *template* mode which means it's interpreted somewhat like
  //   a Scala string interpolation.
  //
  // There are also some additional parameters which may be passed to compilation:
  // * Variable types - types of additional (typed) variables which may be accessed by an expression. They are carried
  //   through the expression context instead of root object. Variables should be avoided - everything should be passed
  //   through the root object if possible.
  // * Template mode - specifies whether template mode is used. This is `true` by default. If set to `false`, the
  //   expression will be "bare-bone" - it will be directly interpreted as code instead of string interpolation.
  // * Additional header, added after profile header.
  //
  // Note that all of these parameters constitute a cache key in expression caches. Use them carefully so that caching
  // is effective.
  val expression = "${someString.toUpperCase}-${someInt+1}"
  val compiledExpr: Expression[SimpleContext[Root], String] =
    compiler.getCompiledExpression[SimpleContext[Root], String](profile, expression)

  // Now that we have the expression compiled, we can finally *evaluate* it.
  // For that we will need a root object implementation.
  final case class RootImpl(someString: String, someInt: Int) extends Root

  def main(args: Array[String]): Unit = {
    // In order to evaluate an expression, we have to create the root object and wrap it into an expression context.
    // The simplest way to do this is to use `SimpleContext` that keeps a set of mutable variables which may
    // be accessed by the expression.
    val context = SimpleContext[Root](RootImpl("fred", 42))

    // At last, evaluate and print the result!
    println(compiledExpr(context)) // FRED-43
  }
}
