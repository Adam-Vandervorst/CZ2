package be.adamv.cz2

import scala.collection.mutable


enum Instr:
  case AppliedTo(i: Int)
  case Prefix(i: Int)
  case ArgsOfFunc(i: Int)


class ExprMapEngine[V]:
  private def prefix[W](i: Long)(res: ExprMap[W]): ExprMap[ExprMap[W]] =
    ExprMap(if res.isEmpty then null else EM(
      ExprMap(if res.em.apps.em eq null then null else EM(
        prefix(i)(res.em.apps.em.apps),
        if res.em.apps.em.vars.isEmpty then mutable.LongMap.empty else
          mutable.LongMap.single(i, ExprMap(EM(ExprMap(), res.em.apps.em.vars))))),
      if res.em.vars.isEmpty then mutable.LongMap.empty else
        mutable.LongMap.single(i, ExprMap(EM(ExprMap(), res.em.vars)))))

  private def argsOfFunc[W](i: Int)(res: ExprMap[W]): ExprMap[W] =
    ExprMap(EM(res.em.vars.getOrElse(i,
      ExprMap(EM(res.em.apps.em.vars.getOrElse(i,
          argsOfFunc(i)(res.em.apps.em.apps)).asInstanceOf,
        mutable.LongMap.empty))).asInstanceOf,
      mutable.LongMap.empty))


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
          if res.em.apps.nonEmpty then

            res = res.em.vars.getOrElse(i,
              res.em.apps.em.vars.getOrElse(i,
                argsOfFunc(i)(res.em.apps.em.apps))
            ).asInstanceOf[ExprMap[V]]
          else
            res = ExprMap()
    }
    res
