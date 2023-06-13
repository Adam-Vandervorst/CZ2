package be.adamv.cz2

import scala.collection.mutable


enum Instr:
  case Apply(f: Int)
  case Unapply(f: Int)
  case Prepend(head: Int)
  case Tail(head: Int)
  case Drop
  case DropHead


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


  def execute(initial: ExprMap[V], instrs: IterableOnce[Instr]): ExprMap[V] =
    var res: ExprMap[V] = initial
    instrs.iterator.foreach {
      case Instr.Apply(f) =>
        if res.nonEmpty then
          res = ExprMap(EM(ExprMap(EM(ExprMap(), mutable.LongMap.single(f.toLong, res))), mutable.LongMap.empty))
      case Instr.Unapply(f) =>
        if res.nonEmpty then
          res = if res.em.apps.em eq null then ExprMap() else res.em.apps.em.vars(f).asInstanceOf
      case Instr.Prepend(head) =>
        if res.nonEmpty then
          res = ExprMap(EM(prepend(head)(res), mutable.LongMap.empty))
      case Instr.Tail(head) =>
        if res.nonEmpty then
          res = tail(head)(res.em.apps)
      case Instr.Drop =>
        if res.nonEmpty then
          res = if res.em.apps.em eq null then ExprMap() else
            res.em.apps.em.vars.valuesIterator.foldLeft(ExprMap[V]())(_.union(_)).union(
              res.em.apps.em.apps.asInstanceOf[ExprMap[V]]
            ).union(
              if res.em.apps.em.apps.em eq null then ExprMap() else res.em.apps.em.vars.valuesIterator.foldLeft(ExprMap[V]())(_.union(_)).asInstanceOf[ExprMap[V]]
            )
      case Instr.DropHead =>
        if res.nonEmpty then
          res = drophead(res.em.apps)
    }
    res
