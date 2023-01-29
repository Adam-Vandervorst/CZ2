package be.adamv.cz2

import collection.mutable

class ExprMapSolver:
  val parents: ExprMap[ExprMap[Unit]] = ExprMap()
  val links: ExprMap[ExprMap[Unit]] = ExprMap()

  def add_node(e: Expr): Unit =
    parents.updateWithDefault(e)(ExprMap())(identity)

  def create_arc(parent: Expr, son: Expr): Unit =
    parents.updateWithDefault(son)(ExprMap(parent -> ()))(x => {x.update(parent, ()); x})

  def create_link(x: Expr, y: Expr): Unit =
    links.updateWithDefault(x)(ExprMap(y -> ()))(_.updated(y, ()))
    links.updateWithDefault(y)(ExprMap(x -> ()))(_.updated(x, ()))

  def add_arcs(e: Expr): Unit = e match
    case Var(_) => add_node(e)
    case _ => e.foldMap(Var.apply, (f, a) => {
      val app = App(f, a)
      create_arc(app, f)
      create_arc(app, a)
      app
    })

  def solve(oes: Expr*): Map[Int, Expr] =
    oes.foreach(add_arcs)
    oes.reduceLeft((x, y) => { create_link(x, y); y })

    var i = 100
    var symbolChanged = true
    while symbolChanged && i > 0 do
      symbolChanged = false
      parents.foreachKey {
        case Var(i) if i <= 0 => ()
        case e =>
          finish(e)
          symbolChanged = true
      }
      i -= 1
    var varChanged = true
    while varChanged && i > 0 do
      varChanged = false
      parents.foreachKey {
        case v @ Var(i) if i <= 0 =>
          finish(v)
          varChanged = true
        case _ => ()
      }
      i -= 1
    if i <= 0 then throw RuntimeException(s"Hit unification limit $i")
    buildMapping()

  def ret(oes: Expr*): Expr =
    val representative = oes.head
    // TODO no need to build the whole mapping
    val bindings = solve(oes: _*)
    representative.substAbs(bindings)

  val complete: ExprMap[Unit] = ExprMap()
  val pointer: ExprMap[Expr] = ExprMap()
  val subs: mutable.Map[Int, Expr] = mutable.Map.empty
  val ready: ExprMap[Unit] = ExprMap()

  def finish(r: Expr): Unit =
    if complete.contains(r) then return ()
    if pointer.contains(r) then throw java.lang.IllegalStateException("pointer not null")
    val stack: collection.mutable.Stack[Expr] = collection.mutable.Stack(r)
    pointer(r) = r
    while stack.nonEmpty do
      val s = stack.pop()
      if r constantDifferent s then
        throw Solver.Conflict
      for t <- parents.get(s).fold(Iterable.empty)(_.keys) do
        finish(t)
      for t <- links.get(s).fold(Iterable.empty)(_.keys) do
        if complete.contains(t) || t == r then
          parents.remove(t)
        else if !pointer.contains(t) then
          pointer(t) = r
          stack.push(t)
        else if pointer.getUnsafe(t) != r then
          throw Solver.Conflict
        else
          parents.remove(t) // it's already on the stack
      end for
      if s != r then
        s match
          case Var(i) =>
            if i <= 0 then
              subs(i) = r
            else ()
          case App(sf, sa) =>
            val App(rf, ra) = r: @unchecked
            create_link(sf, rf)
            create_link(sa, ra)
        complete.update(s, ())
        parents.remove(s)
      end if
    end while
    complete.update(r, ())
    parents.remove(r)

  def buildMapping(): Map[Int, Expr] =
    subs.map((k, v) => k -> descend(v)).toMap

  def descend(u: Expr): Expr =
    if ready.contains(u) then u
    else u match
      case Var(i) if i <= 0 => subsOrReady(i)
      case Var(_) => u
      case App(f, a) =>
        val out = App(descend(f), descend(a))
        if out == u then ready.update(u, ())
        out

  def subsOrReady(x: Int): Expr =
    subs.get(x) match
      case None =>
        val v = Var(x)
        ready.update(v, ())
        v
      case Some(v) => descend(v)
end ExprMapSolver

object ExprMapSolver extends Solver
