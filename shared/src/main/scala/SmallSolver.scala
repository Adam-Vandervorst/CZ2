package be.adamv.cz2

import collection.mutable

import be.adamv.cz2

// Adapted from https://like-a-boss.net/2020/07/07/logic-programming-in-scala.html

case class Knowledge(map: Map[Int, Expr]):
  /*
   * Modify or introduce a binding `v <- t`
   */
  def modBind(v: Int, t: Expr): Knowledge =
    copy(map.updated(v, t))

  /*
   * Recursively looks up the value of variable `v` (if any)
   */
  def lookup(v: Int): Option[Expr] =
    map.get(v).flatMap(walk)

  /*
   * If `t` is a var or contains a var look it up recursively.
   */
  private def walk(t: Expr): Option[Expr] = t match
    case Expr.Var(i) if i < 0 => lookup(i)
    case Expr.App(f, a) => Some(Expr.App(walk(f).getOrElse(f), walk(a).getOrElse(a)))
    case v => Some(v)

object Knowledge:
  def empty = Knowledge(Map.empty[Int, Expr])


object Unification:
  /*
   * Unify t1 with t2 assuming no knowledge.
   */
  extension (t1: Expr)
    infix def unify(t2: Expr): Option[Knowledge] =
      unifyWith(t1, t2, Knowledge.empty)

  /*
   * Unify t1 with t2 using/improving the passed knowledge.
   */
  def unifyWith(t1: Expr, t2: Expr, knowledge: Knowledge): Option[Knowledge] = (t1, t2) match
    case (Var(i), Var(j)) if i < 0 && j < 0 =>
      if i == j then Some(knowledge)
      else bind(i, t2, knowledge) // left-biased binding
    case (Var(i), _) if i < 0 =>
      bind(i, t2, knowledge)
    case (_, Var(j)) if j < 0 =>
      bind(j, t1, knowledge)
    case (Expr.Var(i), Expr.Var(j)) if i > 0 && j > 0 =>
      Some(knowledge)
    case (Expr.App(lf, la), Expr.App(rf, ra)) =>
      for kf <- unifyWith(lf, rf, knowledge)
          ka <- unifyWith(la, ra, kf)
        yield ka
  end unifyWith

  /*
   * Trying to unify `V` with `Expr(V, b, c)` should fail as this is equivalent to infinite regress.
   */
  def bind(v: Int, t: Expr, knowledge: Knowledge): Option[Knowledge] =
    if occursCheck(v, knowledge, t) then
      None
    else
      knowledge.lookup(v) match
        case None =>
          Some(knowledge.modBind(v, t))
        case Some(otherT) =>
          unifyWith(t, otherT, knowledge).map(_.modBind(v, t))
  end bind

  /*
   * Recursively checks if `v` occurs in `t` using the supplied knowledge.
   */
  def occursCheck(v: Int, knowledge: Knowledge, t: Expr): Boolean =
    def reachlist(l: List[Int]): List[Int] =
      l ::: l.flatMap(reachable)
    def reachable(v: Int): List[Int] =
      reachlist(knowledge.lookup(v).fold(List.empty)(vars))

    // v may be aliased to some other variable
    val aliased = knowledge.lookup(v) match
      case Some(Var(i)) if i < 0 => i
      case _ => v

    reachlist(vars(t)).contains(aliased)
  end occursCheck

  private def vars(t: Expr): List[Int] = t match
    case Expr.App(f, a) => vars(f) ::: vars(a)
    case Expr.Var(i) if i < 0 => i::Nil
    case _ => Nil
end Unification


@main def m =
  import Unification.*
  import ExprExamples.{f, g, h, a, b, c, $x, $y, $z}

  val ex1 = (Expr($x, $y) unify Expr($y, a)).get
  val ex2 = (Expr($x, a, $x) unify Expr(Expr(a, b), $y, Expr($y, b))).get

  println(ex1)
  println(ex2)
