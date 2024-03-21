package be.adamv.cz2

import scala.collection.mutable


private sealed trait EMImpl[V, F[_]]:
  def copy(): F[V]
  def contains(e: Expr): Boolean
  def getUnsafe(e: Expr): V
  def get(e: Expr): Option[V]
  def getAt(ins: IterableOnce[Option[Int]]): Either[EM[_], V]
  def updated(e: Expr, v: V): F[V]
  def update(e: Expr, v: V): Unit
  def updateWithDefault(e: Expr)(default: => V)(f: V => V): Unit
  def updateWith(e: Expr)(f: Option[V] => Option[V]): Unit
  def remove(e: Expr): Option[V]
  def keys: Iterable[Expr]
  def values: Iterable[V]
  def items: Iterable[(Expr, V)]
  def union(that: F[V]): F[V]
  def unionWith(op: (V, V) => V)(that: F[V]): F[V]
  def intersection(that: F[V]): ExprMap[V]
  def intersectionWith(op: (V, V) => V)(that: F[V]): ExprMap[V]
  def subtract(that: F[V]): ExprMap[V]
  def subtractWith(p: (V, V) => Option[V])(that: F[V]): ExprMap[V]
  def foreachKey(f: Expr => Unit): Unit
  def foreachItem(f: (Expr, V) => Unit): Unit
  def foreach(f: V => Unit): Unit
  def map[W](f: V => W): F[W]
  def collect[W](pf: PartialFunction[V, W]): F[W]
  def indiscriminateMatching(e: Expr): ExprMap[V]
  def indiscriminateReverseMatching(e: Expr): ExprMap[V]
  def indiscriminateBidirectionalMatching(e: Expr): ExprMap[V]
//  def matching(e: Expr, tracker: ExprMap[mutable.ArrayDeque[Int]]): ExprMap[V]
  def transform(pattern: Expr, template: Expr): ExprMap[V]
  def transformMatches(pattern: Expr, template: Expr): ExprMap[V]
  def flatMap[W](op: (W, W) => W)(f: V => ExprMap[W]): ExprMap[W]
  def foldRight[R](z: R)(op: (V, R) => R): R
  def size: Int
  def prettyStructured(colored: Boolean = true, tree: Boolean = false): String
  def prettyStructuredSet(colored: Boolean = true, tree: Boolean = false): String
  def prettyListing(colored: Boolean = true): String
  def json: String
  def isEmpty: Boolean
  def nonEmpty: Boolean = !isEmpty
end EMImpl

final case class EM[V](apps: ExprMap[ExprMap[V]],
                   var vars: VarMap[V]) extends EMImpl[V, EM]:
  def copy(): EM[V] = EM(apps.copy(), vars.copy())

  def contains(e: Expr): Boolean = e match
    case Var(i) => vars.contains(i)
    case App(f, a) => apps.get(f).fold(false)(_.contains(a))

  def updated(e: Expr, v: V): EM[V] =
    val c = this.copy()
    c.update(e, v)
    c

  def update(e: Expr, v: V): Unit = e match
    case Var(i) =>
      vars = vars.updated(i, v)
    case App(f, a) =>
      apps.updateWithDefault(f)(ExprMap.single(a, v)){
        gapp => gapp.update(a, v); gapp
      }

  def updateWithDefault(e: Expr)(default: => V)(remap: V => V): Unit = e match
    case Var(i) =>
      vars = vars.updatedWithDefault(i, default, remap)
    case App(f, a) =>
      apps.updateWithDefault(f)(ExprMap.single(a, default)){
        gapp => gapp.updateWithDefault(a)(default)(remap); gapp
      }

  def updateWith(e: Expr)(remap: Option[V] => Option[V]): Unit = e match
    case Var(i) =>
      vars.updatedWith(i)(remap)
    case App(f, a) =>
      apps.updateWith(f){
        case Some(gapp) => gapp.updateWith(a)(remap); Some(gapp)
        case None => val c = ExprMap[V](); c.updateWith(a)(remap); Some(c)
      }

  def remove(e: Expr): Option[V] = e match
    case Var(i) => val v = vars.get(i); vars = vars.removed(i); v
    case App(f, a) =>
      apps.get(f).flatMap(_.remove(a))

  def getUnsafe(e: Expr): V = e match
    case Var(i) => vars(i)
    case App(f, a) => apps.getUnsafe(f).getUnsafe(a)

  def get(e: Expr): Option[V] = e match
    case Var(i) => vars.get(i)
    case App(f, a) => apps.get(f).flatMap(_.get(a))

  def getAt(ins: IterableOnce[Option[Int]]): Either[EM[_], V] =
    var loc: EM[_] = this
    var apps = 0
    var it = ins.iterator
    while it.hasNext do it.next() match
      case Some(s) =>
        if apps > 0 then
          val temp = loc.vars.getOrElse(s, throw java.util.NoSuchElementException(f"failed to get an ExprMap at $s from ${loc.prettyStructured()} (depth $apps)")).asInstanceOf[ExprMap[_]]
          if temp.isEmpty then
            println(f"tried to get var $s on $loc at depth $apps and got empty")
            it = Iterator.empty
          loc = temp.em
          apps -= 1
        else if apps == 0 then
          val v = loc.vars.getOrElse(s, throw java.util.NoSuchElementException(f"failed to get a value at $s from ${loc.prettyStructured()}")).asInstanceOf[V]
          val n = it.nextOption()
          assert(it.isEmpty, s"iterator still had ${n.get} but we are at value level ${v}")
          return Right(v)
      case None =>
        val temp = loc.apps
        assert(temp.nonEmpty, f"got to $apps deep and there's nothing deeper")
        loc = temp.em
        apps += 1
    Left(loc)

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

  def union(that: EM[V]): EM[V] =
    EM(
      this.apps.unionWith(_ union _)(that.apps),
      this.vars.union(that.vars)
    )

  def unionWith(op: (V, V) => V)(that: EM[V]): EM[V] =
    EM(
      this.apps.unionWith(_.unionWith(op)(_))(that.apps),
      this.vars.unionWith(that.vars, (_, l, r) => op(l, r))
    )

  def intersection(that: EM[V]): ExprMap[V] =
    val vs = vars.intersection(that.vars)
      .filter{ case (_, v: ExprMap[_]) => v.nonEmpty; case _ => true } // TODO hack
    val as = this.apps.intersectionWith(_.intersection(_))(that.apps)
    ExprMap[V](if vs.isEmpty && as.isEmpty then null else EM(as, vs))

  def intersectionWith(op: (V, V) => V)(that: EM[V]): ExprMap[V] =
    val vs = vars.intersectionWith(that.vars, (_, l, r) => op(l, r))
      .filter{ case (_, v: ExprMap[_]) => v.nonEmpty; case _ => true } // TODO hack
    val as = this.apps.intersectionWith(_.intersectionWith(op)(_))(that.apps)
    ExprMap[V](if vs.isEmpty && as.isEmpty then null else EM(as, vs))

  def subtract(that: EM[V]): ExprMap[V] =
    val vs = vars.subtract(that.vars)
    val as = this.apps.subtractWith((x, y) => Some(x.subtract(y)).filter(_.nonEmpty))(that.apps)
    ExprMap[V](if vs.isEmpty && as.isEmpty then null else EM(as, vs))

  def subtractWith(p: (V, V) => Option[V])(that: EM[V]): ExprMap[V] =
    val vs = vars.subtractWith(p)(that.vars)
    val as = this.apps.subtractWith((x, y) => Some(x.subtractWith(p)(y)).filter(_.nonEmpty))(that.apps)
    ExprMap[V](if vs.isEmpty && as.isEmpty then null else EM(as, vs))

  def map[W](f: V => W): EM[W] = EM(
    apps.map(_.map(f)),
    vars.mapValuesNow(f)
  )

  def collect[W](pf: PartialFunction[V, W]): EM[W] =
    EM(
      apps.collect(((em: ExprMap[V]) =>
        val r = em.collect(pf)
        if r.nonEmpty then Some(r) else None
      ).unlift),
      vars.modifyOrRemove((_, r) => pf.unapply(r))
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
    case Var(i) if i > 0 => vars.get(i).fold(ExprMap())(x => ExprMap.single(Var(i), x))
    case Var(_) => ExprMap(this)
    case App(f, a) =>
      val lv1: ExprMap[ExprMap[V]] = apps.indiscriminateMatching(f)
      val lv2: ExprMap[ExprMap[V]] = lv1.map[ExprMap[V]](_.indiscriminateMatching(a))
      ExprMap(EM(lv2, VarMap.empty))

  def indiscriminateReverseMatching(e: Expr): ExprMap[V] = e match
    case Var(i) => ExprMap(EM(ExprMap(), vars.filter((j, _) => j <= 0 || j == i)))
    case App(f, a) =>
      val lv1: ExprMap[ExprMap[V]] = apps.indiscriminateReverseMatching(f)

      ExprMap(EM(lv1.map[ExprMap[V]] { (nem: ExprMap[V]) =>
        nem.indiscriminateReverseMatching(a)
      }, vars.filter((j, _) => j <= 0)))

  def indiscriminateBidirectionalMatching(e: Expr): ExprMap[V] = e match
    case Var(i) if i > 0 =>
//      println(s"removing apps and symbols != ${i} from ${this.prettyStructuredSet()}")
      ExprMap(EM(ExprMap(), vars.filter((j, _) => j <= 0 || j == i)))
    case Var(_) => ExprMap(this)
    case App(f, a) =>
      val lv1: ExprMap[ExprMap[V]] = apps.indiscriminateBidirectionalMatching(f)
//      println(s"stepping into apps removing symbols from ${this.prettyStructuredSet()}")
      ExprMap(EM(lv1.collect[ExprMap[V]](((nem: ExprMap[V]) =>
        val r = nem.indiscriminateBidirectionalMatching(a)
        if r.nonEmpty then Some(r) else None
      ).unlift), vars.filter((j, _) => j <= 0)))


  def transform(pattern: Expr, template: Expr): ExprMap[V] =
    val possible = indiscriminateBidirectionalMatching(pattern)

    ExprMap.from(possible.items.collect(((x: (Expr, V)) =>
      util.Try(x._1.transform(pattern, template) -> x._2).toOption
    ).unlift))

  def transformMatches(pattern: Expr, template: Expr): ExprMap[V] =
    val possible = indiscriminateBidirectionalMatching(pattern)

    val res = ExprMap[V]()
    possible.foreachItem((e, v) =>
      e.transformMatchesM(pattern, template).foreach(res.update(_, v))
    )
    res

  inline def execute(instrs: IterableOnce[Instr]): ExprMap[V] = ExprMap(this).execute(instrs)

  def flatMap[W](op: (W, W) => W)(f: V => ExprMap[W]): ExprMap[W] =
    var nem = apps.flatMap(op)(_.flatMap(op)(f))
    vars.foreachValue(v => nem = nem.unionWith(op)(f(v)))
    nem

  def foldRight[R](z: R)(op: (V, R) => R): R =
    var a = z
    apps.foreach{ v => a = v.foldRight(a)(op) }
    vars.foreachValue{ v => a = op(v, a) }
    a

  def foreachKey(func: Expr => Unit): Unit =
    vars.foreachKey(k => func(Var(k.toInt)))
    apps.foreachItem((f, em) =>
      em.foreachKey(a => func(App(f, a)))
    )

  def foreachItem(func: (Expr, V) => Unit): Unit =
    vars.foreachEntry((k, v) => func(Var(k.toInt), v))
    apps.foreachItem((f, em) =>
      em.foreachItem((a, v) => func(App(f, a), v))
    )

  def foreach(f: V => Unit): Unit =
    vars.foreachValue(f)
    apps.foreach(_.foreach(f))

  def size: Int = foldRight(0)((_, c) => c + 1)

  inline def prettyStructured(colored: Boolean = true, tree: Boolean = false): String = ExprMap(this).prettyStructured(colored, tree)
  inline def prettyStructuredSet(colored: Boolean = true, tree: Boolean = false): String = ExprMap(this).prettyStructuredSet(colored, tree)
  inline def prettyListing(colored: Boolean = true): String = ExprMap(this).prettyListing(colored)
  inline def json: String = ExprMap(this).json

  def isEmpty: Boolean = vars.isEmpty && apps.isEmpty
end EM

object EM:
  final def single[V](e: Expr, v: V): EM[V] = e match
    case Var(i) => EM(ExprMap(), VarMap.single(i, v))
    case App(f, a) => EM(
      ExprMap.single(f, ExprMap.single(a, v)),
      VarMap.empty
    )

//  def single[V](e: Expr, v: V): EM[V] = e match
//    case Var(i) => EM(ExprMap(), VarMap.single(i, v))
//    case App(f, a) => EM(
//      f match
//        case Var(i) => ExprMap(EM(ExprMap(), VarMap.single(i, ExprMap.single(a, v))))
//        case App(f2, a2) => ExprMap(EM(
//          ExprMap.single(f2, ExprMap.single(a2, ExprMap.single(a, v))),
//          VarMap.empty
//        )),
//      VarMap.empty
//    )

final case class ExprMap[V](var em: EM[V] = null) extends EMImpl[V, ExprMap]:
  def copy(): ExprMap[V] = if em eq null then ExprMap() else ExprMap(em.copy())
  def contains(e: Expr): Boolean = if em eq null then false else em.contains(e)
  inline def getUnsafe(e: Expr): V = em.getUnsafe(e)
  def get(e: Expr): Option[V] = if em eq null then None else em.get(e)
  def getAt(ins: IterableOnce[Option[Int]]): Either[EM[_], V] = if em eq null then ins.iterator.nextOption().fold(Left(null)){ ins => throw RuntimeException(s"tried to execute ${ins} on empty") } else em.getAt(ins)
  def updated(e: Expr, v: V): ExprMap[V] = ExprMap(if em eq null then EM.single(e, v) else em.updated(e, v))
  inline def update(e: Expr, v: V): Unit = if em eq null then em = EM.single(e, v) else em.update(e, v)
  inline def updateWithDefault(e: Expr)(default: => V)(f: V => V): Unit = if em eq null then em = EM.single(e, default) else em.updateWithDefault(e)(default)(f)
  def updateWith(e: Expr)(f: Option[V] => Option[V]): Unit = if em eq null then f(None).foreach(v => em = EM.single(e, v)) else em.updateWith(e)(f)
  def remove(e: Expr): Option[V] = if em eq null then None else em.remove(e)
  def keys: Iterable[Expr] = if em eq null then Iterable.empty else em.keys
  def values: Iterable[V] = if em eq null then Iterable.empty else em.values
  def items: Iterable[(Expr, V)] = if em eq null then Iterable.empty else em.items
  inline def union(that: ExprMap[V]): ExprMap[V] = if em eq null then that.copy() else if that.em eq null then this.copy() else ExprMap(this.em.union(that.em))
  inline def unionWith(op: (V, V) => V)(that: ExprMap[V]): ExprMap[V] = if em eq null then that.copy() else if that.em eq null then this.copy() else ExprMap(this.em.unionWith(op)(that.em))
  inline def intersection(that: ExprMap[V]): ExprMap[V] = if (em eq null) || (that.em eq null) then ExprMap() else this.em.intersection(that.em)
  inline def intersectionWith(op: (V, V) => V)(that: ExprMap[V]): ExprMap[V] = if (em eq null) || (that.em eq null) then ExprMap() else this.em.intersectionWith(op)(that.em)
  inline def subtract(that: ExprMap[V]): ExprMap[V] = if em eq null then this.copy() else if that.em eq null then this.copy() else this.em.subtract(that.em)
  inline def subtractWith(p: (V, V) => Option[V])(that: ExprMap[V]): ExprMap[V] = if em eq null then this.copy() else if that.em eq null then this.copy() else this.em.subtractWith(p)(that.em)
  def foreachKey(f: Expr => Unit): Unit = if em ne null then em.foreachKey(f)
  def foreachItem(f: (Expr, V) => Unit): Unit = if em ne null then em.foreachItem(f)
  def foreach(f: V => Unit): Unit = if em ne null then em.foreach(f)
  def map[W](f: V => W): ExprMap[W] = ExprMap(if em eq null then null else em.map(f))
  def collect[W](pf: PartialFunction[V, W]): ExprMap[W] = ExprMap(if em eq null then null else em.collect(pf))
  def indiscriminateMatching(e: Expr): ExprMap[V] = if em eq null then ExprMap() else em.indiscriminateMatching(e)
  def indiscriminateReverseMatching(e: Expr): ExprMap[V] = if em eq null then ExprMap() else em.indiscriminateReverseMatching(e)
  def indiscriminateBidirectionalMatching(e: Expr): ExprMap[V] = if em eq null then ExprMap() else em.indiscriminateBidirectionalMatching(e)
  //  def matching(e: Expr, tracker: ExprMap[mutable.ArrayDeque[Int]] = ExprMap()): ExprMap[V] = if em == null then ExprMap() else em.matching(e, tracker)
  def execute(instrs: IterableOnce[Instr], debug: 0 | 1 | 2 = 0): ExprMap[V] = new ExprMapEngine[V].execute(this, instrs, debug)
  def transform(pattern: Expr, template: Expr): ExprMap[V] = if em eq null then ExprMap() else em.transform(pattern, template)
  def transformMatches(pattern: Expr, template: Expr): ExprMap[V] = if em eq null then ExprMap() else em.transformMatches(pattern, template)
  def flatMap[W](op: (W, W) => W)(f: V => ExprMap[W]): ExprMap[W] = if em eq null then ExprMap() else em.flatMap(op)(f)
  def foldRight[R](z: R)(op: (V, R) => R): R = if em eq null then z else em.foldRight(z)(op)

  private def appliedRec[W](fem: ExprMap[W], aem: ExprMap[W]): ExprMap[ExprMap[W]] =
    ExprMap(if fem.em eq null then null else EM[ExprMap[W]](
      ExprMap(if fem.em.apps.em eq null then null else EM[ExprMap[ExprMap[W]]](
        appliedRec(fem.em.apps.em.apps, aem.asInstanceOf),
        fem.em.apps.em.vars.mapValuesNow(appliedRec(_, aem)))),
      fem.em.vars.mapValuesNow(_ => aem)))

  def applied(aem: ExprMap[V]): ExprMap[V] =
    ExprMap(EM(appliedRec(this, aem), VarMap.empty))

  private def appliedWithRec[W](op: (W, W) => W)(fem: ExprMap[W], aem: ExprMap[W]): ExprMap[ExprMap[W]] =
    ExprMap(if fem.em eq null then null else EM(
      ExprMap(if fem.em.apps.em eq null then null else EM(
        appliedWithRec[ExprMap[ExprMap[W]]](_.unionWith(_.unionWith(op)(_))(_))(fem.em.apps.em.apps, aem.asInstanceOf),
        fem.em.apps.em.vars.mapValuesNow(em => appliedWithRec(op)(em, aem)))),
      fem.em.vars.mapValuesNow(w1 => aem.map(w2 => op(w1, w2)))))

  def appliedWith(op: (V, V) => V)(aem: ExprMap[V]): ExprMap[V] =
    ExprMap(EM(appliedWithRec(op)(this, aem), VarMap.empty))

  def appliedWithSim[W](op: (W, W) => W)(fem: ExprMap[W])(aem: ExprMap[W]): ExprMap[W] =
    ExprMap.from(fem.items.flatMap((fe, fw) => aem.items.map((ae, aw) => Expr(fe, ae) -> op(fw, aw))))

  def size: Int = if em eq null then 0 else foldRight(0)((_, c) => c + 1)
  def prettyStructured(colored: Boolean = true, tree: Boolean = false): String = EMPrettyPrinter.structured(this, colored=colored, tree=tree)
  def prettyStructuredSet(colored: Boolean = true, tree: Boolean = false): String = EMPrettyPrinter.structuredSet(this, colored=colored, tree=tree)
  def prettyListing(colored: Boolean = true): String = EMListPrinter.listing(this, colored=colored)
  def json: String = EMJSONPrinter.structured(this, colored=false)
  inline def isEmpty: Boolean = (em eq null) || em.isEmpty
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
  inline def single[V](e: Expr, v: V): ExprMap[V] = ExprMap(EM.single(e, v))
  inline def vars[V](vars: Array[Long], values: Array[V]): ExprMap[V] =
    ExprMap(EM(ExprMap(), VarMap.fromZip(vars, values)))
