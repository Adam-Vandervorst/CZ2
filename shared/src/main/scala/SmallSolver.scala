package be.adamv.cz2

import collection.mutable

import be.adamv.cz2


extension (e1: Expr)
  infix def unifyRel(e2: Expr): Option[(Seq[Expr], Seq[Expr])] =
    val lvars = mutable.ArrayDeque.empty[Expr]
    val rvars = mutable.ArrayDeque.empty[Expr]

    val leqs = mutable.Map.empty[Int, Int]
    val reqs = mutable.Map.empty[Int, Int]

    def rec(l: Expr, r: Expr, lv: Int): Boolean =
      println(s"U$lv ${l.pretty()} ${r.pretty()}")
      (l, r) match
        case (Var(i), Var(j)) if i > 0 && j > 0 =>
          i == j
        case (Var(0), Var(0)) =>
          val lc = lvars.length
          val rc = rvars.length
          lvars.append(r)
          rvars.append(l)
          leqs(lc) = rc
          reqs(rc) = lc
          true
        case (Var(i), Var(j)) if i < 0 && j < 0 =>
          if lvars.contains(~i) && rvars.contains(~j) then
            lvars(~i) == rvars(~j)
          else
            leqs(~i) = ~j
            reqs(~j) = ~i
            true
        case (Var(0), Var(j)) if j < 0 =>
          if rvars.contains(~j) then
            lvars.append(rvars(~j))
            true
          else
            val lc = lvars.length
            leqs(lc) = ~j
            reqs(~j) = lc
            true
        case (Var(i), Var(0)) if i < 0 =>
          if lvars.contains(~i) then
            rvars.append(lvars(~i))
            true
          else
            val rc = rvars.length
            reqs(rc) = ~i
            leqs(~i) = rc
            true
        case (_, Var(j)) if j < 0 =>
          if reqs.contains(~j) then
            println(f"AR ${Var(j).pretty()} to ${l.pretty()}  (${lvars.map(_.pretty()).mkString(",")})")
            if ~j < rvars.length then rvars(~j) = l.substPossibleRel(lvars.toSeq)
            else if ~j == rvars.length then rvars.append(l)
            else println(f"ERROR R adding ${~j} to ${rvars}")
            true
          else if ~j < rvars.length then
            val res = l.substPossibleRel(lvars.toSeq) == rvars(~j)
            if !res then println(f"rvar ${Var(j).pretty()} ${l.pretty()} != ${rvars(~j).pretty()}  (${lvars.map(_.pretty()).mkString(",")})")
            res
          else
            println("ryikes")
            false
        case (Var(i), _) if i < 0 =>
          if leqs.contains(~i) then
            println(f"AL ${Var(i).pretty()} to ${r.pretty()}  (${rvars.map(_.pretty()).mkString(",")})")
            if ~i < lvars.length then lvars(~i) = r.substPossibleRel(rvars.toSeq)
            else if ~i == lvars.length then lvars.append(r)
            else println(f"ERROR L adding ${Var(i).pretty()} to ${rvars}")
            true
          else if ~i < lvars.length then
            val res = lvars(~i) == r.substPossibleRel(rvars.toSeq)
            if !res then println(f"lvar ${Var(i).pretty()} ${lvars(~i).pretty()} != ${r.pretty()}  (${rvars.map(_.pretty()).mkString(",")})")
            res
          else
            println(s"lyikes ${Var(i).pretty()}  ${r.pretty()}  ${lvars}")
            false
        case (_, Var(0)) =>
          println(f"FR ${rvars.length} := ${l.pretty()}  (${lvars.map(_.pretty()).mkString(",")})")
          rvars.append(l.substPossibleRel(lvars.toSeq))
          true
        case (Var(0), _) =>
          println(f"FL ${lvars.length} := ${r.pretty()}  (${rvars.map(_.pretty()).mkString(",")})")
          lvars.append(r.substPossibleRel(rvars.toSeq))
          true
        case (App(lf, la), App(rf, ra)) =>
          rec(lf, rf, lv + 1) && rec(la, ra, lv + 1)
        case _ => throw IllegalStateException()

    Option.when(rec(e1, e2, 0))((lvars.toSeq, rvars.toSeq))


@main def m =
  import ExprExamples.{f, g, h, a, b, c, $, _1, _2, _3}

  def quickShow(r: Option[(Seq[Expr], Seq[Expr])]): Unit = r match
    case None => println("empty")
    case Some((l, r)) =>
      println("l: " + l.map(_.pretty()).mkString(", "))
      println("r: " + r.map(_.pretty()).mkString(", "))

//  assert((Expr($, $) unifyRel Expr($, a)).contains((List(Var(0), Var(10)), List(Var(0)))))
//  assert((Expr($, _1) unifyRel Expr(a, b)).isEmpty)
//  assert((Expr($, _1) unifyRel Expr(a, a)).contains((List(Var(10)),List())))
//  assert((Expr(Expr(a, b), $) unifyRel Expr(Expr(a, $), c)).contains((List(Var(12)),List(Var(11)))))
//  assert((Expr($, a, _1) unifyRel Expr($, _1, a)).contains((List(Var(10)),List(Var(10)))))
//  assert((Expr($, _1, a, _1) unifyRel Expr($, _1, _1, a)).contains((List(Var(10)),List(Var(10)))))
//  assert((Expr(a, $) unifyRel Expr($, Expr(_1, b))).contains((List(App(Var(10),Var(11))),List(Var(10)))))
//  assert((Expr($, a, _1) unifyRel Expr(Expr(a, b), $, Expr(_1, b))).contains((List(App(Var(10),Var(11))),List(Var(10)))))
//  assert((Expr($, a, _1) unifyRel Expr(Expr(b, $), $, Expr(_2, _1))).isEmpty)
//  assert((Expr(a, Expr(a, $), Expr(a, _1, $)) unifyRel Expr($, Expr(_1, b), Expr(_1, b, c))).contains((List(Var(11), Var(12)),List(Var(10)))))
//  assert((Expr.nest(f, g, h, a, $) unifyRel Expr.nest(f, g, h, $, b)).contains((List(Var(11)),List(Var(10)))))