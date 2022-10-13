package be.adamv

import collection.mutable

enum Expr:
  case Var(i: Int)
  case App(f: Expr, a: Expr)

  def size: Int = foldMap(_ => 1, _ + _ + 1)

  def fvars: Seq[Int] = foldMap(i => if i > 0 then Seq(i) else Seq(), _ ++ _)

  def foldMap[A](varf: Int => A, appf: (A, A) => A): A = this match
    case Var(i) => varf(i)
    case App(f, a) => appf(f.foldMap(varf, appf), a.foldMap(varf, appf))

  def subst(mapping: Seq[Expr]): Expr =
    var index = 0
    def rec(e: Expr): Expr = e match
      case Var(i) if i > 0 => e
      case Var(0) =>
        val v = mapping(index)
        index += 1
        v
      case Var(i) => mapping(~i)
      case App(f, a) => App(rec(f), rec(a))
    rec(this)

  def toAbsolute(offset: Int): Expr =
    val vars: mutable.ArrayDeque[Int] = mutable.ArrayDeque.empty
    def rec(e: Expr): Expr = e match
      case Var(i) if i > 0 => e
      case Var(0) =>
        val n = -vars.length - offset
        vars.append(n)
        Var(n)
      case Var(n) => Var(vars(~n))
      case App(f, a) =>
        App(rec(f), rec(a))
    rec(this)

  def toRelative: Expr =
    val vars: mutable.ArrayDeque[Int] = mutable.ArrayDeque.empty
    def rec(e: Expr): Expr = e match
      case Var(i) if i > 0 => e
      case Var(n) =>
        val i = vars.indexOf(n)
        if i == -1 then
          vars.append(n)
          Var(0)
        else
          Var(~i)
      case App(f, a) =>
        App(rec(f), rec(a))
    rec(this)

  infix def matches(that: Expr): Option[(Seq[Expr], Seq[Expr])] =
    val lvars = mutable.ArrayDeque.empty[Expr]
    val rvars = mutable.ArrayDeque.empty[Expr]
    def rec(l: Expr, r: Expr): Boolean = (l, r) match
      case (Var(i), Var(j)) if i > 0 && j > 0 =>
        i == j
      case (Var(0), Var(0)) =>
        lvars.append(r)
        rvars.append(l)
        true
      case (_, Var(0)) =>
        rvars.append(l)
        true
      case (Var(0), _) =>
        lvars.append(r)
        true
      case (Var(i), Var(j)) if i < 0 && j < 0 =>
        lvars(~i) == rvars(~j)
      case (_, Var(j)) if j < 0 =>
        l == rvars(~j)
      case (Var(i), _) if i < 0 =>
        lvars(~i) == r
      case (App(lf, la), App(rf, ra)) =>
        rec(lf, rf) && rec(la, ra)
    Option.when(rec(this, that))((lvars.toSeq, rvars.toSeq))

  infix def constantDifferent(that: Expr): Boolean = (this, that) match
    case (Var(i), Var(j)) if i > 0 && j > 0 => i != j
    case _ => false

  infix def unifiable(that: Expr): Boolean =
    val solver = new Solver()
    try
      solver.solve(this, that)
      //      println(solver.subs)
      true
    catch case Solver.Conflict =>
      //      println("conflict")
      false
export Expr.*

object Expr:
  def apply(es: Expr*): Expr = es.reduce(App(_, _))

  def unify(tup: Tuple): Map[Expr, Expr] =
    val s = new Solver
    val sosl = s.solve(tup)
    //    println(sosl)
    //    println(s.subs)
    sosl

extension (inline sc: StringContext)
  inline def eids(inline args: Any*): String =
    StringContext.standardInterpolator(identity, args.map{
      case Expr.Var(i) => i.toString
      case a: Expr.App => throw RuntimeException("Only vars have id's")
      case x => x
    }, sc.parts)
