package be.adamv

import collection.mutable

class EMSolver:
  val parents: ExprMap[ExprMap[Unit]] = ExprMap()
  val links: ExprMap[ExprMap[Unit]] = ExprMap()
  val em: ExprMap[Unit] = ExprMap()

  def add_node(e: Expr): Unit =
    parents.updateWithDefault(e)(ExprMap())(identity)

  def create_arc(parent: Expr, son: Expr): Unit =
    parents.updateWithDefault(son)(ExprMap(parent -> ()))(x => {x.update(parent, ()); x})

  def create_link(x: Expr, y: Expr): Unit =
    links.updateWithDefault(x)(ExprMap(y -> ()))(_.updated(y, ()))
    links.updateWithDefault(y)(ExprMap(x -> ()))(_.updated(x, ()))

  def nodes: Set[Expr] = parents.keys.toSet

  def add_arcs(e: Expr): Unit = e match
    case Var(_) => add_node(e)
    case _ => e.foldMap(Var.apply, (f, a) => {
      val app = App(f, a)
      create_arc(app, f)
      create_arc(app, a)
      app
    })

  def solve(oes: Tuple): Map[Expr, Expr] =
    oes.productIterator.mapAccumulate(100){ case (e: Expr, offset: Int) =>
      val ea = e.toAbsolute(offset)
      em(ea) = ()
      add_arcs(ea)
      (ea, offset + 100)
    }.reduceLeft((x, y) => { create_link(x, y); y })

    var i = 20
    while nodes.exists{ case v @ Var(i) if i > 0 => !ignore(v); case a @ App(_, _) => !ignore(a); case _ => false } && i > 0 do
      //      println("some f symbols")
      val nem = ExprMap.from(nodes.filter{ case v @ Var(i) if i > 0 => !ignore(v); case a @ App(_, _) => !ignore(a); case _ => false }.map(_ -> ()))
      finish(nem)
      i -= 1
    while nodes.exists{ case v @ Var(i) if i <= 0 => !ignore(v); case _ => false } && i > 0 do
      //      println("some vars")
      val nem = ExprMap.from(nodes.filter{ case v @ Var(i) if i <= 0 => !ignore(v); case _ => false }.map(_ -> ()))
      finish(nem)
      i -= 1
    buildMapping()

  val complete: mutable.Set[Expr] = mutable.Set.empty
  val ignore: mutable.Set[Expr] = mutable.Set.empty
  val pointer: mutable.Map[Expr, Expr] = mutable.Map.empty.withDefaultValue(null)
  val subs: mutable.Map[Expr, Expr] = mutable.Map.empty
  val ready: mutable.Set[Expr] = mutable.Set.empty

  def finish(r: ExprMap[Unit]): Unit =
    if r.isEmpty then return
    println(f"finish $r")
    if r.keys.forall(complete) then return
    if r.keys.forall(pointer(_) != null) then throw java.lang.IllegalStateException("pointer not null")
    val stack: ExprMap[Unit] = r.copy()
    r.keys.foreach(x => pointer(x) = x)
    while stack.keys.nonEmpty do
      val s = stack.keys.head
      if r.keys.forall(_ constantDifferent s) then
      //        println(f"constant different $r $s")
        throw Solver.Conflict
      parents.get(s).fold(())(finish)
      links.get(s).fold(()){ tem =>
        tem.keys.foreach{ t =>
          if complete(t) || r.contains(t) then
            ignore.add(t)
          else if pointer(t) == null then
            r.keys.foreach(x => pointer(t) = x)
            stack(t) = ()
          else if pointer(t) != r then
            throw Solver.Conflict
          else
            ignore.add(t)
        }
      }
      if s != r then
        s match
          case Var(i) =>
            if i <= 0 then
              ()
//              println(r)
//              subs(s) = r
            else ()
          case App(sf, sa) =>
            r.keys.foreach(x => {
              val App(rf, ra) = x
              create_link(sf, rf)
              create_link(sa, ra)
            })
        complete.add(s)
        ignore.add(s)
      end if
    end while
    r.keys.foreach(x => {complete.add(x); ignore.add(x)})

  def buildMapping(): Map[Expr, Expr] =
    subs.map((k, v) => k -> descend(v)).toMap

  def descend(u: Expr): Expr =
    if ready(u) then u
    else u match
      case Var(i) if i <= 0 => subsOrReady(u)
      case Var(_) => u
      case App(f, a) =>
        val out = App(descend(f), descend(a))
        if out == u then ready.add(u)
        out

  def subsOrReady(x: Expr): Expr =
    subs.get(x) match
      case None => ready.add(x); x
      case Some(v) => descend(v)
end EMSolver

object EMSolver extends Solver
