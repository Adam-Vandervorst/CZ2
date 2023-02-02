package be.adamv.cz2

import scala.collection.mutable


enum Instr:
  case AppliedTo(i: Int)
  case Prefix(i: Int)
  case ArgsOfFunc(i: Int)


class ExprMapEngine[V]:
  private def prefix[W](i: Long)(res: ExprMap[W]): ExprMap[ExprMap[W]] =
    ExprMap(if res.em eq null then null else EM(
      ExprMap(if res.em.apps.em eq null then null else EM(
        prefix(i)(res.em.apps.em.apps),
        if res.em.apps.em.vars.isEmpty then mutable.LongMap.empty else
          mutable.LongMap.single(i, ExprMap(EM(ExprMap(), res.em.apps.em.vars))))),
      if res.em.vars.isEmpty then mutable.LongMap.empty else
        mutable.LongMap.single(i, ExprMap(EM(ExprMap(), res.em.vars)))))

  private def argsOfFunc[W](i: Int)(res: ExprMap[ExprMap[W]]): ExprMap[W] =
    ExprMap(if res.em eq null then null else EM(
      ExprMap(if res.em.apps.em eq null then null else EM(
        argsOfFunc(i)(res.em.apps.em.apps),
        res.em.apps.em.vars.get(i).fold(mutable.LongMap.empty)(_.em.vars))),
      res.em.vars.get(i).fold(mutable.LongMap.empty)(_.em.vars)))


  def execute(initial: ExprMap[V], instrs: IterableOnce[Instr]): ExprMap[V] =
    var res: ExprMap[V] = initial
    instrs.iterator.foreach {
      case Instr.AppliedTo(i) =>
        if res.nonEmpty then
          res = ExprMap(EM(ExprMap(EM(ExprMap(), mutable.LongMap.single(i.toLong, res))), mutable.LongMap.empty))
      case Instr.Prefix(i) =>
        if res.nonEmpty then
          res = ExprMap(EM(prefix(i)(res), mutable.LongMap.empty))
      case Instr.ArgsOfFunc(i) =>
        if res.nonEmpty then
          res = if res.em.apps.nonEmpty then argsOfFunc(i)(res.asInstanceOf).em.apps.asInstanceOf else ExprMap()
    }
    res
