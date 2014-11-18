import scala.reflect.macros.Universe

def validateSyntax(u: Universe)(tree: u.Tree): (Boolean, List[u.Tree]) = {
  import u._

  def isLambdaParamDef(valDef: ValDef) =
    valDef.mods.hasFlag(Flag.PARAM) && valDef.rhs == EmptyTree

  tree match {
    case _: Block | _: Select | _: Apply | _: TypeApply | _: Ident |
         _: If | _: Literal | _: New | _: This | _: Typed | _: TypTree =>
      (true, tree.children)
    case Function(valDefs, body) if valDefs.forall(isLambdaParamDef) =>
      (true, body :: valDefs.map(_.tpt))
    case _ => (false, tree.children)
  }
}
