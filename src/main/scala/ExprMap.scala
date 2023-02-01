package be.adamv.cz2

import scala.collection.mutable


enum Instr:
  case AppliedTo(i: Int)
  case Prefix(i: Int)

def prefix[W](i: Long)(res: EM[W]): EM[ExprMap[W]] =
  EM(ExprMap(if res.apps.em eq null then null else EM(
      ExprMap(if res.apps.em.apps.em eq null then null else
        prefix(i)(res.apps.em.apps.em)),
      if res.apps.em.vars.isEmpty then mutable.LongMap.empty else
        mutable.LongMap.single(i, ExprMap(EM(ExprMap(), res.apps.em.vars))))),
    if res.vars.isEmpty then mutable.LongMap.empty else
      mutable.LongMap.single(i, ExprMap(EM(ExprMap(), res.vars))))


private sealed trait EMImpl[V, F[_]]:
  def copy(): F[V]
  def contains(e: Expr): Boolean
  def getUnsafe(e: Expr): V
  def get(e: Expr): Option[V]
  def updated(e: Expr, v: V): F[V]
  def update(e: Expr, v: V): Unit
  def updateWithDefault(e: Expr)(default: => V)(f: V => V): Unit
  def updateWith(e: Expr)(f: Option[V] => Option[V]): Unit
  def remove(e: Expr): Option[V]
  def keys: Iterable[Expr]
  def values: Iterable[V]
  def items: Iterable[(Expr, V)]
  def merge(op: (V, V) => V)(that: F[V]): F[V]
  def foreachKey(f: Expr => Unit): Unit
  def foreachItem(f: (Expr, V) => Unit): Unit
  def foreach(f: V => Unit): Unit
  def map[W](f: V => W): F[W]
  def collect[W](pf: PartialFunction[V, W]): F[W]
  def indiscriminateMatching(e: Expr): ExprMap[V]
  def indiscriminateReverseMatching(e: Expr): ExprMap[V]
  def indiscriminateBidirectionalMatching(e: Expr): ExprMap[V]
//  def matching(e: Expr, tracker: ExprMap[mutable.ArrayDeque[Int]]): ExprMap[V]
  def execute(instrs: IterableOnce[Instr]): F[V]
  def transform(pattern: Expr, template: Expr): ExprMap[V]
  def transformMatches(pattern: Expr, template: Expr): ExprMap[V]
  def flatMap[W](op: (W, W) => W)(f: V => ExprMap[W]): ExprMap[W]
  def foldRight[R](z: R)(op: (V, R) => R): R
  def size: Int
  def prettyStructured(colored: Boolean = true): String
  def prettyStructuredSet(colored: Boolean = true): String
  def prettyListing(colored: Boolean = true): String
  def json: String
  def isEmpty: Boolean
  def nonEmpty: Boolean = !isEmpty
end EMImpl

case class EM[V](apps: ExprMap[ExprMap[V]],
                 vars: mutable.LongMap[V]) extends EMImpl[V, EM]:
  def copy(): EM[V] = EM(apps.copy(), mutable.LongMap.from(vars))

  def contains(e: Expr): Boolean = e match
    case Var(i) => vars.contains(i)
    case App(f, a) => apps.get(f).fold(false)(_.contains(a))

  def updated(e: Expr, v: V): EM[V] =
    val c = this.copy()
    c.update(e, v)
    c

  def update(e: Expr, v: V): Unit = e match
    case Var(i) =>
      vars.update(i, v)
    case App(f, a) =>
      apps.updateWithDefault(f)(ExprMap().updated(a, v)){
        gapp => gapp.update(a, v); gapp
      }

  def updateWithDefault(e: Expr)(default: => V)(remap: V => V): Unit = e match
    case Var(i) =>
      vars.update(i, vars.get(i) match
        case None => default
        case Some(v) => remap(v)
      )
    case App(f, a) =>
      apps.updateWithDefault(f)(ExprMap().updated(a, default))(
        gapp => { gapp.updateWithDefault(a)(default)(remap);  gapp }
      )

  def updateWith(e: Expr)(remap: Option[V] => Option[V]): Unit = e match
    case Var(i) =>
      vars.updateWith(i)(remap)
    case App(f, a) =>
      apps.updateWith(f){
        case Some(gapp) => gapp.updateWith(a)(remap); Some(gapp)
        case None => val c = ExprMap[V](); c.updateWith(a)(remap); Some(c)
      }

  def remove(e: Expr): Option[V] = e match
    case Var(i) => vars.remove(i)
    case App(f, a) =>
      apps.get(f).flatMap(_.remove(a))

  def getUnsafe(e: Expr): V = e match
    case Var(i) => vars(i)
    case App(f, a) => apps.getUnsafe(f).getUnsafe(a)

  def get(e: Expr): Option[V] = e match
    case Var(i) => vars.get(i)
    case App(f, a) => apps.get(f).flatMap(_.get(a))

  def keys: Iterable[Expr] =
    val ks = mutable.ArrayDeque.empty[Expr]
    foreachKey(ks.append)
    ks

  def items: Iterable[(Expr, V)] =
    val ks = mutable.ArrayDeque.empty[(Expr, V)]
    foreachItem((k, v) => ks.append(k -> v))
    ks

  def values: Iterable[V] =
    val vs = mutable.ArrayDeque.empty[V]
    foreach(vs.append)
    vs

  def merge(op: (V, V) => V)(that: EM[V]): EM[V] =
    EM(
      this.apps.merge((emvthis, emvthat) => emvthis.merge(op)(emvthat))(that.apps),
      mutable.LongMap.from(this.vars.toMap.merge(op)(that.vars.toMap))
    )

  def map[W](f: V => W): EM[W] = EM(
    apps.map(_.map(f)),
    vars.mapValuesNow(f)
  )

  def collect[W](pf: PartialFunction[V, W]): EM[W] = EM(
    apps.collect(_.collect(pf)),
    vars.collect{ case (k, pf(r)) => k -> r }
  )

  //  def keysMatching(e: Expr, bindings: mutable.Map[Int, Expr]): EM[(V, Int)] = e match
  //    case Var(i) if i < 0 => vars.get(i) match
  //      case Some(v) => EM(ExprMap(), mutable.Map(i -> (v, i)))
  //      case None => null
  //    case Var(0) => EM(ExprMap(), mutable.Map.from(vars.ma))
  //    case Var(b) =>
  //      keysMatching(bindings(b), bindings)
  //    case App(f, a) =>
  //      if apps == null then null
  //      else
  //        apps.matching(f).flatMap()(emv => emv.matching(a)).em


  //          case App(f, a) => ???
  //            val pat_var_bndr: Set[(PatSubst, V)] = pvar match
  //              case Some(v) => Set((psubst.extend(e), v))
  //              case None => Set.empty
  //
  //            val pat_var_occs: Set[(PatSubst, V)] = Set.from(
  //              for (pat_var, v) <- xvar
  //                  if e == psubst.lookup(pat_var)
  //              yield (psubst, v))
  //
  //            val look_at_e: Set[(PatSubst, V)] = e match
  //              case Variable(x) => fvar.get(x) match
  //                case Some(v) => Set((psubst, v))
  //                case None => Set.empty
  //              case Application(f, t) => lkMExpr(f)(psubst, app).flatMap(lkMExpr(t))
  //
  //            pat_var_bndr union pat_var_occs union look_at_e


  def indiscriminateMatching(e: Expr): ExprMap[V] = e match
    case Var(i) if i > 0 => vars.get(i).fold(ExprMap())(x => ExprMap(Var(i) -> x))
    case Var(_) => ExprMap(this)
    case App(f, a) =>
      val lv1: ExprMap[ExprMap[V]] = apps.indiscriminateMatching(f)

      ExprMap(EM(lv1.map[ExprMap[V]] { (nem: ExprMap[V]) =>
        nem.indiscriminateMatching(a)
      }, collection.mutable.LongMap()))


  def indiscriminateReverseMatching(e: Expr): ExprMap[V] = e match
    case Var(i) => ExprMap(EM(ExprMap(), vars.filter((j, _) => j <= 0 || j == i)))
    case App(f, a) =>
      val lv1: ExprMap[ExprMap[V]] = apps.indiscriminateReverseMatching(f)

      ExprMap(EM(lv1.map[ExprMap[V]] { (nem: ExprMap[V]) =>
        nem.indiscriminateReverseMatching(a)
      }, vars.filter((j, _) => j <= 0)))

  def indiscriminateBidirectionalMatching(e: Expr): ExprMap[V] = e match
    case Var(i) if i > 0 => ExprMap(EM(ExprMap(), vars.filter((j, _) => j <= 0 || j == i)))
    case Var(_) => ExprMap(this)
    case App(f, a) =>
      val lv1: ExprMap[ExprMap[V]] = apps.indiscriminateBidirectionalMatching(f)

      ExprMap(EM(lv1.map[ExprMap[V]] { (nem: ExprMap[V]) =>
        nem.indiscriminateBidirectionalMatching(a)
      }, vars.filter((j, _) => j <= 0)))

  def transform(pattern: Expr, template: Expr): ExprMap[V] =
    val possible = indiscriminateBidirectionalMatching(pattern)

    ExprMap.from(possible.items.collect(((x: (Expr, V)) =>
      util.Try(x._1.transform(pattern, template) -> x._2).toOption
    ).unlift))

  def transformMatches(pattern: Expr, template: Expr): ExprMap[V] =
    val possible = indiscriminateBidirectionalMatching(pattern)

    ExprMap.from(possible.items.collect(((x: (Expr, V)) =>
      util.Try(x._1.transformMatches(pattern, template) -> x._2).toOption
      ).unlift))

  def execute(instrs: IterableOnce[Instr]): EM[V] =
    var res: EM[V] = this
    instrs.iterator.foreach{
      case Instr.AppliedTo(i) =>
        res = EM(ExprMap(em=EM(ExprMap(), mutable.LongMap.single(i.toLong, ExprMap(res)))), mutable.LongMap.empty)
      case Instr.Prefix(i) =>
        res = EM(ExprMap(prefix(i)(res)), mutable.LongMap.empty)
    }
    res

  def flatMap[W](op: (W, W) => W)(f: V => ExprMap[W]): ExprMap[W] =
    vars.foldLeft(ExprMap[W]())((nem, p) => nem.merge(op)(f(p._2))).merge(op)(
      apps.flatMap(op)(_.flatMap(op)(f))
    )

  def foldRight[R](z: R)(op: (V, R) => R): R =
    vars.values.foldRight(apps.foldRight(z)(_.foldRight(_)(op)))(op)

  def foreachKey(func: Expr => Unit): Unit =
    vars.foreachKey(k => func(Var(k.toInt)))
    apps.foreachItem((f, em) =>
      em.foreachKey(a => func(App(f, a)))
    )

  def foreachItem(func: (Expr, V) => Unit): Unit =
    vars.foreach((k, v) => func(Var(k.toInt), v))
    apps.foreachItem((f, em) =>
      em.foreachItem((a, v) => func(App(f, a), v))
    )

  def foreach(f: V => Unit): Unit =
    vars.values.foreach(f)
    apps.foreach(_.foreach(f))

  def size: Int = foldRight(0)((_, c) => c + 1)

  inline def prettyStructured(colored: Boolean = true): String = ExprMap(this).prettyStructured(colored)
  inline def prettyStructuredSet(colored: Boolean = true): String = ExprMap(this).prettyStructuredSet(colored)
  inline def prettyListing(colored: Boolean = true): String = ExprMap(this).prettyListing(colored)
  inline def json: String = ExprMap(this).json

  def isEmpty: Boolean = vars.isEmpty && apps.isEmpty
end EM

case class ExprMap[V](var em: EM[V] = null) extends EMImpl[V, ExprMap]:
  def copy(): ExprMap[V] = if em == null then ExprMap() else ExprMap(em.copy())
  def contains(e: Expr): Boolean = if em == null then false else em.contains(e)
  inline def getUnsafe(e: Expr): V = em.getUnsafe(e)
  def get(e: Expr): Option[V] = if em == null then None else em.get(e)
  private def updatedEmpty(e: Expr, v: V): EM[V] = e match
    case Var(i) => EM(ExprMap(), mutable.LongMap.single(i, v))
    case App(f, a) => EM(
      ExprMap().updated(f, ExprMap().updated(a, v)),
      mutable.LongMap.empty
    )
  def updated(e: Expr, v: V): ExprMap[V] = ExprMap(if em == null then updatedEmpty(e, v) else em.updated(e, v))
  def update(e: Expr, v: V): Unit = if em == null
  then em = updatedEmpty(e, v)
  else em.update(e, v)

  def updateWithDefault(e: Expr)(default: => V)(f: V => V): Unit = if em == null then em = updatedEmpty(e, default) else em.updateWithDefault(e)(default)(f)
  def updateWith(e: Expr)(f: Option[V] => Option[V]): Unit = if em == null
  then f(None) match
      case Some(v) => em = updatedEmpty(e, v)
      case None => ()
  else em.updateWith(e)(f)
  def remove(e: Expr): Option[V] = if em == null then None else em.remove(e)
  def keys: Iterable[Expr] = if em == null then Iterable.empty else em.keys
  def values: Iterable[V] = if em == null then Iterable.empty else em.values
  def items: Iterable[(Expr, V)] = if em == null then Iterable.empty else em.items
  def merge(op: (V, V) => V)(that: ExprMap[V]): ExprMap[V] =
    if em == null then that.copy()
    else if that.em == null then this.copy()
    else ExprMap(this.em.merge(op)(that.em))
  def foreachKey(f: Expr => Unit): Unit = if em != null then em.foreachKey(f)
  def foreachItem(f: (Expr, V) => Unit): Unit = if em != null then em.foreachItem(f)
  def foreach(f: V => Unit): Unit = if em != null then em.foreach(f)
  def map[W](f: V => W): ExprMap[W] = ExprMap(if em == null then null else em.map(f))
  def collect[W](pf: PartialFunction[V, W]): ExprMap[W] = ExprMap(if em == null then null else em.collect(pf))
  def indiscriminateMatching(e: Expr): ExprMap[V] = if em == null then ExprMap() else em.indiscriminateMatching(e)
  def indiscriminateReverseMatching(e: Expr): ExprMap[V] = if em == null then ExprMap() else em.indiscriminateReverseMatching(e)
  def indiscriminateBidirectionalMatching(e: Expr): ExprMap[V] = if em == null then ExprMap() else em.indiscriminateBidirectionalMatching(e)
  //  def matching(e: Expr, tracker: ExprMap[mutable.ArrayDeque[Int]] = ExprMap()): ExprMap[V] = if em == null then ExprMap() else em.matching(e, tracker)
  def execute(instrs: IterableOnce[Instr]): ExprMap[V] = if em == null then ExprMap() else ExprMap(em.execute(instrs))
  def transform(pattern: Expr, template: Expr): ExprMap[V] = if em == null then ExprMap() else em.transform(pattern, template)
  def transformMatches(pattern: Expr, template: Expr): ExprMap[V] = if em == null then ExprMap() else em.transformMatches(pattern, template)
  def flatMap[W](op: (W, W) => W)(f: V => ExprMap[W]): ExprMap[W] = if em == null then ExprMap() else em.flatMap(op)(f)
  def foldRight[R](z: R)(op: (V, R) => R): R = if em == null then z else em.foldRight(z)(op)
  def size: Int = if em == null then 0 else foldRight(0)((_, c) => c + 1)
  def prettyStructured(colored: Boolean = true): String = EMPrettyPrinter.structured(this, colored=colored)
  def prettyStructuredSet(colored: Boolean = true): String = EMPrettyPrinter.structuredSet(this, colored=colored)
  def prettyListing(colored: Boolean = true): String = EMListPrinter.listing(this, colored=colored)
  def json: String = EMJSONPrinter.structured(this, colored=false)
  def isEmpty: Boolean = if em == null then true else em.isEmpty
end ExprMap

object ExprMap:
  inline def apply[V](x: (Expr, V), xs: (Expr, V)*): ExprMap[V] = from(x +: xs)
  inline def from[V](pairs: IterableOnce[(Expr, V)]): ExprMap[V] =
    val em = ExprMap[V]()
    val it = pairs.iterator
    while it.hasNext do
      val (k, v) = it.next()
      em(k) = v
    em
