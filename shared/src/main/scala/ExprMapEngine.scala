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
  private def prepend[W](x: Long)(xs: ExprMap[W]): ExprMap[ExprMap[W]] =
    ExprMap(if xs.em eq null then null else EM(
      ExprMap(if xs.em.apps.em eq null then null else EM(
        prepend(x)(xs.em.apps.em.apps),
        if xs.em.apps.em.vars.isEmpty then mutable.LongMap.empty else
          mutable.LongMap.single(x, ExprMap(EM(ExprMap(), xs.em.apps.em.vars))))),
      if xs.em.vars.isEmpty then mutable.LongMap.empty else
        mutable.LongMap.single(x, ExprMap(EM(ExprMap(), xs.em.vars)))))

  private def tail[W](x: Int)(xs: ExprMap[ExprMap[W]]): ExprMap[W] =
    ExprMap(if xs.em eq null then null else EM(
      ExprMap(if xs.em.apps.em eq null then null else EM(
        tail(x)(xs.em.apps.em.apps),
        xs.em.apps.em.vars.get(x).fold(mutable.LongMap.empty)(_.em.vars))),
      xs.em.vars.get(x).fold(mutable.LongMap.empty)(_.em.vars)))

  private def drophead[W](xs: ExprMap[ExprMap[W]]): ExprMap[W] =
    ExprMap(if xs.em eq null then null else EM(
      ExprMap(if xs.em.apps.em eq null then null else EM(
        drophead(xs.em.apps.em.apps),
        xs.em.apps.em.vars.valuesIterator.map(_.em.vars).foldLeft(mutable.LongMap.empty)(_.union(_)))),
      xs.em.vars.valuesIterator.map(_.em.vars).foldLeft(mutable.LongMap.empty)(_.union(_))))


  def execute(initial: ExprMap[V], instrs: IterableOnce[Instr], debug: Boolean = false): ExprMap[V] =
    if initial.isEmpty then return initial
    var res: ExprMap[V] = initial
    if debug then println(f"initial ${res.prettyStructuredSet()}")
    val it = instrs.iterator
    while it.hasNext && res.nonEmpty do
      val i = it.next()
      if debug then println(f"executing ${i}")
      i match
      case Instr.Apply(f) =>
        res = ExprMap(EM(ExprMap(EM(ExprMap(), mutable.LongMap.single(f.toLong, res))), mutable.LongMap.empty))
      case Instr.Unapply(f) =>
        res = if res.em.apps.em eq null then ExprMap() else res.em.apps.em.vars(f).asInstanceOf
      case Instr.Prepend(head) =>
        res = ExprMap(EM(prepend(head)(res), mutable.LongMap.empty))
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
        if res.em.apps.nonEmpty then
          res = ExprMap(EM(ExprMap(), res.em.vars))
      case Instr.ClearSymbols =>
        if res.em.vars.nonEmpty then
          res = ExprMap(EM(res.em.apps, res.em.vars.filter((j, _) => j <= 0)))
      case Instr.RestrictSymbols(i) =>
        if res.em.vars.nonEmpty then
          res = ExprMap(EM(res.em.apps, res.em.vars.filter((j, _) => j <= 0 || j == i)))
      case Instr.ZoomInApps =>
        val block = it.takeWhile(_ != Instr.ZoomOutApps).toVector
        if res.em.apps.nonEmpty then
          res = ExprMap(EM(res.em.apps.execute(block, debug), res.em.vars))
      case Instr.ZoomOutApps =>
        ()
      case Instr.CollectApps(is: _*) =>
        if res.em.apps.nonEmpty then
          res = ExprMap(EM(res.em.apps.collect(((em: ExprMap[V]) =>
            val r = em.execute(is, debug)
            if (r.em ne null) && r.em.vars.nonEmpty then Some(r) else None
          ).unlift), res.em.vars))
      if debug then println(f"state ${res.prettyStructuredSet()}")
    if debug then println("final")
    res
