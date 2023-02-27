package be.adamv.cz2

import collection.mutable
import scala.annotation.tailrec

enum Expr:
  case Var(i: Int)
  case App(f: Expr, a: Expr)

  def size: Int = foldMap(_ => 1, _ + _ + 1)

  def fvars: Seq[Int] = foldMap(i => if i > 0 then Seq(i) else Seq(), _ ++ _)
  def bvars: Seq[Int] = foldMap(i => if i < 0 then Seq(i) else Seq(), _ ++ _)
  def nvarsN: Int = foldMap(i => if i == 0 then 1 else 0, _ + _)

  def applyAll(args: Expr*): Expr = args.foldLeft(this)(App(_, _))

  @tailrec
  final def leftMost: Int = this match
    case Var(i) => i
    case App(f, a) => f.leftMost

  def pretty(colored: Boolean = true): String = PrettyPrinter.sexpression(this, colored = colored)

  def show: String = ShowPrinter.sexpression(this, colored = false)

  def foldMap[A](varf: Int => A, appf: (A, A) => A): A = this match
    case Var(i) => varf(i)
    case App(f, a) => appf(f.foldMap(varf, appf), a.foldMap(varf, appf))

  def foldLeftMap[A, B](b: B)(varf: (Int, B) => (A, B), appf: (A, A, B) => (A, B)): (A, B) = this match
    case Var(i) => varf(i, b)
    case App(f, a) => 
      val (fa, fb) = f.foldLeftMap(b)(varf, appf)
      val (aa, ab) = a.foldLeftMap(fb)(varf, appf)
      appf(fa, aa, ab)

  def foldMapAssoc[A](varf: Int => A, exprf: List[A] => A): A = this match
    case Var(i) => varf(i)
    case App(f, a) =>
      var l = a.foldMapAssoc(varf, exprf)::Nil
      var c = f
      while c != null do c match
        case Var(i) =>
          l = varf(i)::l
          c = null
        case App(fk, fa) =>
          l = fa.foldMapAssoc(varf, exprf)::l
          c = fk
      exprf(l)

/*  def get(address: List[Boolean]): Expr = address match
    case Nil => this
    case h::t => this match
      case App(f, a) =>
        if h then a.get(t)
        else f.get(t)
      case _ => null

  def set(address: List[Boolean], value: Expr): Expr = address match
    case Nil => value
    case h::t => this match
      case App(f, a) =>
        if h then App(f, a.set(t, value))
        else App(f.set(t, value), a)
      case _ => null*/

  def substAbs(mapping: Map[Int, Expr]): Expr =
    this.foldMap(x => mapping.getOrElse(x, Var(x)), App(_, _))

  def substPossibleRel(mapping: Seq[Expr]): Expr =
    var index = 0
    val n = mapping.length
    this.foldMap({
      case i if i > 0 => Var(i)
      case 0 =>
        if index < n then
          val v = mapping(index)
          index += 1
          v
        else
          Var(0)
      case i if ~i < n => mapping(~i)
      case i => Var(i)
    }, App(_, _))

  def substRel(mapping: Seq[Expr]): Expr =
    var index = 0
    this.foldMap({
      case i if i > 0 => Var(i)
      case 0 =>
        val v = mapping(index)
        index += 1
        v
      case i => mapping(~i)
    }, App(_, _))

  def bind(n: Int): Expr =
    var index = 0
    this.foldMap(i => Var(if i == 0 then {index += 1; -index - n} else if i > 0 then i else i - n), App(_, _))

  def shift(n: Int): Expr =
    this.foldMap(i => Var(if i >= 0 then i else i - n), App(_, _))

  def substReIndex(mapping: Seq[Expr]): Expr =
    var index = 0
    val additions = mapping.toArray.map(_ => 0)
    this.foldMap({
      case i if i > 0 => Var(i)
      case 0 =>
        val v = mapping(index).shift(additions(index))
        index += 1
        for j <- index until additions.length do
          additions(j) += v.nvarsN
        v
      case i =>
        val v = mapping(~i).bind(additions(~i))
        v
    }, App(_, _))

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
      case _ => throw IllegalStateException()
    Option.when(rec(this, that))((lvars.toSeq, rvars.toSeq))

  infix def constantDifferent(that: Expr): Boolean = (this, that) match
    case (Var(i), Var(j)) if i > 0 && j > 0 => i != j
    case _ => false

  infix def unifiable(that: Expr): Boolean =
    val solver = new ExprMapSolver()
    try
      solver.solve(this.toAbsolute(100), that.toAbsolute(200))
      true
    catch case Solver.Conflict =>
      false

  def transform(pattern: Expr, template: Expr): Expr =
    val data_placeholder = Expr(this, Expr.zero)
    val pattern_template = Expr(pattern, template)
    // what comes in, must come out
    val App(_, res) = Expr.unifyTo(data_placeholder.toAbsolute(100), pattern_template.toAbsolute(200)): @unchecked
    res.toRelative

  def transformMatches(pattern: Expr, template: Expr): Expr =
    val data_placeholder = Expr(this, Expr.zero)
    val pattern_template = Expr(pattern, template)
    // what comes in, must come out
    val Some(lr, rl) = data_placeholder matches pattern_template: @unchecked
    template.substRel(rl).substRel(lr)

  def transformMatchesM(pattern: Expr, template: Expr): Option[Expr] =
    val data_placeholder = Expr(this, Expr.zero)
    val pattern_template = Expr(pattern, template)
    (data_placeholder matches pattern_template).map((lr, rl) => template.substRel(rl).substRel(lr))
export Expr.*

object Expr:
  val zero: Expr = Var(0)
  
  def apply(e1: Expr, e2: Expr, es: Expr*): Expr = (e1 +: e2 +: es).reduceLeft(App(_, _))
  def nest(e1: Expr, e2: Expr, es: Expr*): Expr = (e1 +: e2 +: es).reduceRight(App(_, _))

  def unapplySeq(x: Expr): Option[List[Expr]] = x match
    case App(f, a) =>
      var l = a::Nil
      var c = f
      while c != null do c match
        case Var(i) =>
          l = Var(i)::l
          c = null
        case App(fk, fa) =>
          l = fa::l
          c = fk
      Some(l)
    case _ => None

  def unify(eos: Expr*): Map[Int, Expr] =
    val s = new ExprMapSolver
    val sosl = s.solve(eos: _*)
    //    println(sosl)
    //    println(s.subs)
    sosl

  def unifyTo(eos: Expr*): Expr =
    val s = new ExprMapSolver
    s.ret(eos: _*)

extension (inline sc: StringContext)
  inline def eids(inline args: Any*): String =
    sc.s(args.map{
      case Expr.Var(i) => i.toString
      case a: Expr.App => throw RuntimeException("Only vars have id's")
      case x => x
    }: _*)
