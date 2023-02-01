package be.adamv.cz2

import scala.collection.mutable


enum Instr[+V]:
  case AppliedTo(i: Int)
  case Prefix(i: Int)
  case ArgsOf[VV](merge: (VV, VV) => VV) extends Instr[VV]
  case DropArg[VV](merge: (VV, VV) => VV) extends Instr[VV]


class ExprMapEngine[V]:
  private def prefix[W](i: Long)(res: ExprMap[W]): ExprMap[ExprMap[W]] =
    ExprMap(if res.isEmpty then null else EM(
      ExprMap(if res.em.apps.em eq null then null else EM(
        prefix(i)(res.em.apps.em.apps),
        if res.em.apps.em.vars.isEmpty then mutable.LongMap.empty else
          mutable.LongMap.single(i, ExprMap(EM(ExprMap(), res.em.apps.em.vars))))),
      if res.em.vars.isEmpty then mutable.LongMap.empty else
        mutable.LongMap.single(i, ExprMap(EM(ExprMap(), res.em.vars)))))

  def execute(initial: ExprMap[V], instrs: IterableOnce[Instr[V]]): ExprMap[V] =
    var res: ExprMap[V] = initial
    instrs.iterator.foreach {
      case Instr.AppliedTo(i) =>
        if res.nonEmpty then
          res = ExprMap(EM(ExprMap(EM(ExprMap(), mutable.LongMap.single(i.toLong, res))), mutable.LongMap.empty))
      case Instr.Prefix(i) =>
        if res.nonEmpty then
          res = ExprMap(EM(prefix(i)(res), mutable.LongMap.empty))
      case Instr.ArgsOf(m: ((V, V) => V)) =>
        if res.nonEmpty then
          if res.em.apps.nonEmpty then
            val appsres = if res.em.apps.em.apps.nonEmpty then ExprMap(EM(res.em.apps.em.apps.em.vars.values.foldLeft(ExprMap())((em1: ExprMap[ExprMap[V]], em2: ExprMap[ExprMap[V]]) =>
              em1.merge(_.merge(m)(_))(em2)), mutable.LongMap.empty)) else ExprMap()
            res = res.em.apps.em.vars.values.foldLeft(appsres)(_.merge(m)(_))
          else
            res = ExprMap()
      case Instr.DropArg(m: ((V, V) => V)) =>
        if res.nonEmpty then
          if res.em.apps.nonEmpty then
            res = ExprMap(EM(ExprMap(), mutable.LongMap.fromZip(res.em.apps.em.vars.keys, res.em.apps.em.vars.keys.mapAccumulate(0)((_, i) => (i.asInstanceOf[V], i + 1)))))
          else
            res = ExprMap()
    }
    res
