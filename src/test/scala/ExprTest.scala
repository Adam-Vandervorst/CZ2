package be.adamv

import munit.FunSuite

object ExprExamples:
  import Expr.*
  val f: Expr = Var(1)
  val g: Expr = Var(2)
  val h: Expr = Var(3)

  val a: Expr = Var(10)
  val b: Expr = Var(11)
  val c: Expr = Var(12)

  val `=`: Expr = Var(100)
  val `,`: Expr = Var(101)

  val _1: Expr = Var(-1)
  val _2: Expr = Var(-2)
  val _3: Expr = Var(-3)

  val $: Expr = Var(0)

  // f x a
  val e1: Expr = Expr(f, $, a)
  // f x y y x
  val e2: Expr = Expr(f, $, $, _2, _1)
  // f y (g y x)
  val e3: Expr = Expr(f, $, Expr(g, _1, $))
  // (= (nTimes (, $x (S (S $n)))) (Plus (, $x (S Z)) (, $x (S $n))))
  val r1: Expr = Expr(`=`,
    Expr(f, Expr(`,`, $, Expr(g, Expr(g, $)))),
    Expr(h, Expr(`,`, _1, Expr(g, a)), Expr(`,`, _1, Expr(g, _2)))
  )


class ExprTest extends FunSuite:
  import ExprExamples.*

  test("foldMap size fvars") {
    assert(e1.size == 5)
    assert(e1.fvars == Seq(1, 10))
    assert(e2.size == 9)
    assert(e2.fvars == Seq(1))
    assert(e3.size == 9)
    assert(e2.fvars == Seq(1))
  }

  test("toAbsolute fromAbsolute") {
    val e1a = Expr(f, Expr.Var(-100), a)
    val e2a = Expr(f, Expr.Var(-100), Expr.Var(-101), Expr.Var(-101), Expr.Var(-100))
    val e3a = Expr(f, Expr.Var(-100), Expr(g, Expr.Var(-100), Expr.Var(-101)))
    assert(e1.toAbsolute(100) == e1a)
    assert(e2.toAbsolute(100) == e2a)
    assert(e3.toAbsolute(100) == e3a)
    assert(e1a.toRelative == e1)
    assert(e2a.toRelative == e2)
    assert(e3a.toRelative == e3)
    assert(r1.toAbsolute(100).toRelative == r1)
    assert(r1.toAbsolute(100).toRelative.toAbsolute(200) == r1.toAbsolute(200))
  }

  test("matchable") {
    assert((a matches a).contains((List(), List())))
    assert((a matches b).isEmpty)
    assert(($ matches $).contains((List($),List($))))
    assert((Expr(a, b) matches Expr(a, b)).contains((List(), List())))
    assert((Expr(a, b) matches Expr(a, c)).isEmpty)
    assert((Expr($, b) matches Expr(a, b)).contains((List(a),List())))
    assert((Expr(Expr($, a), Expr(_1, b)) matches Expr(Expr(c, a), Expr(c, b))).contains((List(c),List())))
    assert((Expr(Expr($, a), Expr(_1, b)) matches Expr(Expr(c, a), Expr(a, b))).isEmpty)
    assert((Expr(Expr($, a), Expr(_1, b)) matches Expr(Expr(a, $), Expr(a, _1))).isEmpty)
    assert((Expr(Expr($, a), Expr(_1, a)) matches Expr(Expr(b, $), Expr(b, _1))).contains((List(b), List(a))))
    // println(Expr(Expr($, a), Expr(_1, b)) matches Expr(Expr($, a), Expr(b, _1)))
    assert((Expr(Expr(a, $), Expr(_1, b)) matches Expr(Expr($, a), Expr(_1, b))).contains((List(a),List(a))))
    // println(Expr($, _1, a, _1) matches Expr($, _1, _1, a))
    assert((Expr($, _1, a, _1) matches Expr($, _1, _1, b)).isEmpty)
    assert((Expr($, _1, a, _1) matches Expr($, _1, _1, b)).isEmpty)
    assert((Expr($, a, _1) matches Expr(Expr(b, $), $, Expr(_2, _1))).isEmpty)
    // println(Expr($, a, _1) matches Expr(Expr(b, $), $, Expr($, _1)))
  }

  test("unifiable") {
    assert(a unifiable a)
    assert(!(a unifiable b))
    assert($ unifiable $)
    assert(Expr(a, b) unifiable Expr(a, b))
    assert(!(Expr(a, b) unifiable Expr(a, c)))
    assert(Expr($, b) unifiable Expr(a, b))
    assert(Expr(Expr($, a), Expr(_1, b)) unifiable Expr(Expr(c, a), Expr(c, b)))
    assert(!(Expr(Expr($, a), Expr(_1, b)) unifiable Expr(Expr(c, a), Expr(a, b))))
    assert(!(Expr(Expr($, a), Expr(_1, b)) unifiable Expr(Expr(a, $), Expr(a, _1))))
    assert(Expr(Expr($, a), Expr(_1, a)) unifiable Expr(Expr(b, $), Expr(b, _1)))
    assert(Expr(Expr($, a), Expr(_1, b)) unifiable Expr(Expr($, a), Expr(b, _1)))
    assert(Expr(Expr(a, $), Expr(_1, b)) unifiable Expr(Expr($, a), Expr(_1, b)))
    assert(Expr($, _1, a, _1) unifiable Expr($, _1, _1, a))
    assert(!(Expr($, _1, a, _1) unifiable Expr($, _1, _1, b)))
    assert(!(Expr($, _1, a, _1) unifiable Expr($, _1, _1, b)))
    assert(!(Expr($, a, _1) unifiable Expr(Expr(b, $), $, Expr(_2, _1))))
    assert(Expr($, a, _1) unifiable Expr(Expr(b, $), $, Expr($, _1)))
  }

  test("unify multiple") {
    Expr.unify(Expr($, a, _1), $, Expr(Expr(a, b), $, Expr(_1, b)))
    // -100 -> App(Var(10), Var(11))
    // -200 -> App(App(App(Var(10), Var(11)), Var(10)), App(Var(10), Var(11)))
    // -300 -> Var(10)
    Expr.unify(
      Expr(a, Expr(a, $), Expr(a, _1, $)),
      Expr($, Expr(_1, b), Expr(_1, b, $)),
      Expr($, Expr(_1, $), Expr(_1, _2, c)),
    )
    Expr.unify(
      Expr(f, a),
      Expr($, a),
      Expr(f, $)
    )
    val `->` = Expr.Var(1000)
    val list = Expr.Var(1001)
    val int = Expr.Var(1002)
    Expr.unify(
      Expr(->, $, Expr(->, Expr(list, _1), Expr(list, _1))),
      Expr(->, $, Expr(->, Expr(list, $), $)),
      Expr(->, int, Expr(->, $, $))
    )
    Expr.unify(
      Expr(Expr(f, $), Expr($, $)),
      Expr(Expr($, a), Expr($, $)),
      Expr(Expr(f, $), Expr(g, $)),
      Expr(Expr(f, $), Expr($, b))
    )
  }

  test("subst") {
    assert(e1.subst(Seq(b)) == Expr(f, b, a))
    assert(e2.subst(Seq(a, b)) == Expr(f, a, b, b, a))
    assert(e3.subst(Seq(a, b)) == Expr(f, a, Expr(g, a, b)))
  }

  test("large subst") {
    import Expr.*
    val `5` = Var(200)
    val Z = Var(201)
    assert(r1.subst(Seq(`5`, App(g,App(g,Z)))) == App(App(`=`,App(f,App(App(`,`,`5`),App(g,App(g,App(g,App(g,Z))))))),App(App(h,App(App(`,`,`5`),App(g,a))),App(App(`,`,`5`),App(g,App(g,App(g,Z)))))))
  }

  test("show pretty") {
    assert(e1.show == "Expr(Var(1),Var(0),Var(10))")
    assert(e2.show == "Expr(Var(1),Var(0),Var(0),Var(-2),Var(-1))")
    assert(e3.show == "Expr(Var(1),Var(0),Expr(Var(2),Var(-1),Var(0)))")
    assert(e1.pretty== "(1 ◆ 10)")
    assert(e2.pretty == "(1 ◆ ◆ ⏴₂ ⏴₁)")
    assert(e3.pretty == "(1 ◆ (2 ⏴₁ ◆))")
  }
