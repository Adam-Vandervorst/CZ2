package be.adamv.cz2

import scala.collection.mutable


enum Instr:
  case Apply(f: Int)
  case Unapply(f: Int)
  case Prepend(head: Int)
  case Tail(head: Int)
  case Drop
  case DropHead

  case ClearApps
  case ClearSymbols
  case RestrictSymbols(f: Int)

  case ZoomInApps
  case ZoomOutApps

  case CollectApps(is: Instr*)


class ExprMapEngine[V]:
  def prepend[W](x: Long)(xs: ExprMap[W]): ExprMap[ExprMap[W]] =
    ExprMap(if xs.em eq null then null else EM(
      ExprMap(if xs.em.apps.em eq null then null else EM(
        prepend(x)(xs.em.apps.em.apps),
        if xs.em.apps.em.vars.isEmpty then VarMap.empty else
          VarMap.single(x, ExprMap(EM(ExprMap(), xs.em.apps.em.vars))))),
      if xs.em.vars.isEmpty then VarMap.empty else
        VarMap.single(x, ExprMap(EM(ExprMap(), xs.em.vars)))))

  def tail[W](x: Int)(xs: ExprMap[ExprMap[W]]): ExprMap[W] =
    ExprMap(if xs.em eq null then null else EM(
      ExprMap(if xs.em.apps.em eq null then null else EM(
        tail(x)(xs.em.apps.em.apps),
        xs.em.apps.em.vars.get(x).fold(VarMap.empty)(_.em.vars))),
      xs.em.vars.get(x).fold(VarMap.empty)(_.em.vars)))

  def drophead[W](xs: ExprMap[ExprMap[W]]): ExprMap[W] =
    ExprMap(if xs.em eq null then null else EM(
      ExprMap(if xs.em.apps.em eq null then null else EM(
        drophead(xs.em.apps.em.apps),
        xs.em.apps.em.vars.valuesIterator.map(_.em.vars).foldLeft(VarMap.empty)(_.union(_)))),
      xs.em.vars.valuesIterator.map(_.em.vars).foldLeft(VarMap.empty)(_.union(_))))


  def execute(initial: ExprMap[V], instrs: IterableOnce[Instr], debug: 0 | 1 | 2 = 0): ExprMap[V] =
    if initial.isEmpty then return initial
    var res: ExprMap[V] = initial
    if debug >= 2 then println(f"initial ${res.prettyStructuredSet()}")
    val it = instrs.iterator
    while it.hasNext do
      val i = it.next()
      if debug >= 1 then println(f"executing ${i}")
      i match
      case Instr.Apply(f) =>
        res = ExprMap(EM(ExprMap(EM(ExprMap(), VarMap.single(f.toLong, res))), VarMap.empty))
      case Instr.Unapply(f) =>
        res = if res.em.apps.em eq null then ExprMap() else res.em.apps.em.vars(f).asInstanceOf
      case Instr.Prepend(head) =>
        res = ExprMap(EM(prepend(head)(res), VarMap.empty))
      case Instr.Tail(head) =>
        res = tail(head)(res.em.apps)
      case Instr.Drop =>
        res = if res.em.apps.em eq null then ExprMap() else
          res.em.apps.em.vars.valuesIterator.foldLeft(ExprMap[V]())(_.union(_)).union(
            res.em.apps.em.apps.asInstanceOf[ExprMap[V]]
          ).union(
            if res.em.apps.em.apps.em eq null then ExprMap() else res.em.apps.em.vars.valuesIterator.foldLeft(ExprMap[V]())(_.union(_)).asInstanceOf[ExprMap[V]]
          )
      case Instr.DropHead =>
        res = drophead(res.em.apps)
      case Instr.ClearApps =>
        res = ExprMap(EM(ExprMap(), res.em.vars))
      case Instr.ClearSymbols =>
        res = ExprMap(EM(res.em.apps, res.em.vars.filter((j, _) => j <= 0)))
      case Instr.RestrictSymbols(i) =>
        res = ExprMap(EM(res.em.apps, res.em.vars.filter((j, _) => j <= 0 || j == i)))
      case Instr.ZoomInApps =>
        res = ExprMap(EM(res.em.apps.execute(it, debug), res.em.vars))
      case Instr.ZoomOutApps =>
        return res
      case Instr.CollectApps(is: _*) =>
        res = ExprMap(EM(res.em.apps.collect(((em: ExprMap[V]) =>
          val r = em.execute(is, debug)
          if r.nonEmpty then Some(r) else None
        ).unlift), res.em.vars))
      if debug >= 2 then println(f"state ${res.prettyStructuredSet()}")
    if debug >= 2 then println("final")
    res
