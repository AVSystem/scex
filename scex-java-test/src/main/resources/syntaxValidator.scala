import scala.reflect.macros.Universe

def isSyntaxAllowed(u: Universe)(tree: u.Tree): Boolean = {
  import u._

  tree match {
    case _: Block | _: Select | _: Apply | _: TypeApply | _: Ident |
         _: If | _: Literal | _: New | _: This | _: Typed | _: TypTree => true
    case _ => false
  }
}
