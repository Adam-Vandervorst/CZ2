package be.adamv

import collection.mutable


class Solver:
  val parents: mutable.Map[Expr, mutable.Set[Expr]] = mutable.Map.empty.withDefault(_ => mutable.Set.empty)
  val links: mutable.Map[Expr, mutable.Set[Expr]] = mutable.Map.empty.withDefault(_ => mutable.Set.empty)

  def add_node(e: Expr): Unit =
    if parents(e).isEmpty then parents(e) = mutable.Set.empty

  def create_arc(parent: Expr, son: Expr): Unit =
    parents(son) = parents(son).addOne(parent)

  def create_link(x: Expr, y: Expr): Unit =
    links(x) = links(x).addOne(y)
    links(y) = links(y).addOne(x)

  def nodes: Set[Expr] = parents.keySet.toSet

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
      add_arcs(ea)
      (ea, offset + 100)
    }.reduceLeft((x, y) => { create_link(x, y); y })

    var i = 20
    while nodes.exists{ case v @ Var(i) if i > 0 => !ignore(v); case a @ App(_, _) => !ignore(a); case _ => false } && i > 0 do
      //      println("some f symbols")
      nodes.filter{ case v @ Var(i) if i > 0 => !ignore(v); case a @ App(_, _) => !ignore(a); case _ => false }.foreach(finish)
      i -= 1
    while nodes.exists{ case v @ Var(i) if i <= 0 => !ignore(v); case _ => false } && i > 0 do
      //      println("some vars")
      nodes.filter{ case v @ Var(i) if i <= 0 => !ignore(v); case _ => false }.foreach(finish)
      i -= 1
    buildMapping()

  val complete: mutable.Set[Expr] = mutable.Set.empty
  val ignore: mutable.Set[Expr] = mutable.Set.empty
  val pointer: mutable.Map[Expr, Expr] = mutable.Map.empty.withDefaultValue(null)
  val subs: mutable.Map[Expr, Expr] = mutable.Map.empty
  val ready: mutable.Set[Expr] = mutable.Set.empty

  def finish(r: Expr): Unit =
    //    println(f"finish $r")
    if complete(r) then return
    if pointer(r) != null then throw java.lang.IllegalStateException("pointer not null")
    val stack: collection.mutable.Stack[Expr] = collection.mutable.Stack(r)
    pointer(r) = r
    while stack.nonEmpty do
      val s = stack.pop()
      if r constantDifferent s then
      //        println(f"constant different $r $s")
        throw Solver.Conflict
      for t <- parents(s) do
        finish(t)
      for t <- links(s) do
        if complete(t) || t == r then
          ignore.add(t)
        else if pointer(t) == null then
          pointer(t) = r
          stack.push(t)
        else if pointer(t) != r then
        //          println(f"pointer of $t (${pointer(t)}) does not point to $r")
          throw Solver.Conflict
        else
          ignore.add(t) // it's already on the stack
      end for
      if s != r then
        s match
          case Var(i) =>
            if i <= 0 then
              subs(s) = r
            else ()
          case App(sf, sa) =>
            val App(rf, ra) = r
            create_link(sf, rf)
            create_link(sa, ra)
        complete.add(s)
        ignore.add(s)
      end if
    end while
    complete.add(r)
    ignore.add(r)

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
end Solver

object Solver:
  object Conflict extends RuntimeException("do not match")
