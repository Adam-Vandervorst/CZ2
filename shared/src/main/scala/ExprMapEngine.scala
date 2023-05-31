package be.adamv.cz2


enum Instr:
  case Apply(f: Int)
  case Unapply(f: Int)
  case Prepend(head: Int)
  case Tail(head: Int)


class ExprMapEngine[V]:
  private def prepend[W](x: Int)(xs: ExprMap[W]): ExprMap[ExprMap[W]] =
    ExprMap(if xs.em eq null then null else EM(
      ExprMap(if xs.em.apps.em eq null then null else EM(
        prepend(x)(xs.em.apps.em.apps),
        if xs.em.apps.em.vars.isEmpty then VarMap.empty else
          VarMap.singleton(x, ExprMap(EM(ExprMap(), xs.em.apps.em.vars))))),
      if xs.em.vars.isEmpty then VarMap.empty else
        VarMap.singleton(x, ExprMap(EM(ExprMap(), xs.em.vars)))))

  private def tail[W](x: Int)(xs: ExprMap[ExprMap[W]]): ExprMap[W] =
    ExprMap(if xs.em eq null then null else EM(
      ExprMap(if xs.em.apps.em eq null then null else EM(
        tail(x)(xs.em.apps.em.apps),
        xs.em.apps.em.vars.get(x).fold(VarMap.empty)(_.em.vars))),
      xs.em.vars.get(x).fold(VarMap.empty)(_.em.vars)))


  def execute(initial: ExprMap[V], instrs: IterableOnce[Instr]): ExprMap[V] =
    var res: ExprMap[V] = initial
    instrs.iterator.foreach {
      case Instr.Apply(f) =>
        if res.nonEmpty then
          res = ExprMap(EM(ExprMap(EM(ExprMap(), VarMap.singleton(f, res))), VarMap.empty))
      case Instr.Unapply(f) =>
        if res.nonEmpty then
          res = if res.em.apps.em eq null then ExprMap() else res.em.apps.em.vars(f).asInstanceOf
      case Instr.Prepend(head) =>
        if res.nonEmpty then
          res = ExprMap(EM(prepend(head)(res), VarMap.empty))
      case Instr.Tail(head) =>
        if res.nonEmpty then
          res = tail(head)(res.em.apps)
    }
    res
