package be.adamv.cz2

import munit.FunSuite

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

  test("toAbsolute toRelative") {
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

  test("substReIndex") {
    val r1 = Expr(f, $, _1)
    assert(r1.substReIndex(Seq($)) == r1)
    assert(r1.substReIndex(Seq(Expr(a, $, $))) == Expr(f, Expr(a, $, $), Expr(a, _1, _2)))
    val r2 = Expr(f, $, $, _1)
    assert(r2.substReIndex(Seq(Expr(a, $, $), A)) == Expr(f, Expr(a, $, $), A, Expr(a, _1, _2)))
    assert(r2.substReIndex(Seq(Expr(a, $, $), $)) == Expr(f, Expr(a, $, $), $, Expr(a, _1, _2)))
    assert(r2.substReIndex(Seq(Expr(a, $, _1), $)) == Expr(f, Expr(a, $, _1), $, Expr(a, _1, _1)))
    val r3 = Expr(`,`, Expr(f, $, $), Expr(g, _2, $, _3))
    assert(r3.substReIndex(Seq(a, b, c)) == Expr(`,`, Expr(f, a, b), Expr(g, b, c, c)))
    assert(r3.substReIndex(Seq(a, $, c)) == Expr(`,`, Expr(f, a, $), Expr(g, _1, c, c)))
    assert(r3.substReIndex(Seq(a, $, $)) == Expr(`,`, Expr(f, a, $), Expr(g, _1, $, _2)))
    assert(r3.substReIndex(Seq($, $, $)) == Expr(`,`, Expr(f, $, $), Expr(g, _2, $, _3)))
    assert(r3.substReIndex(Seq(a, Expr(B, $, $), c)) == Expr(`,`, Expr(f, a, Expr(B, $, $)), Expr(g, Expr(B, _1, _2), c, c)))
    assert(r3.substReIndex(Seq($, Expr(B, $, $), $)) == Expr(`,`, Expr(f, $, Expr(B, $, $)), Expr(g, Expr(B, _2, _3), $, _4)))
    assert(r3.substReIndex(Seq($, Expr(B, $, _1), c)) == Expr(`,`, Expr(f, $, Expr(B, $, _2)), Expr(g, Expr(B, _2, _2), c, c)))
    assert(r3.substReIndex(Seq(Expr(A, $, $), Expr(B, $, _1), c)) == Expr(`,`, Expr(f, Expr(A, $, $), Expr(B, $, _3)), Expr(g, Expr(B, _3, _3), c, c)))
    assert(r3.substReIndex(Seq(Expr(A, $, $), Expr(B, $, $, _2), Expr(C, $, _1))) == Expr(`,`, Expr(f, Expr(A, $, $), Expr(B, $, $, _4)), Expr(g, Expr(B, _3, _4, _4), Expr(C, $, _5), Expr(C, _5, _5))))
  }

  test("matches") {
    /*
    for all matching lhs, rhs
    val Some((lhs_vars, rhs_vars)) = (lhs matches rhs)
    assert(lhs.substRel(lhs_vars) == rhs.substRel(rhs_vars))
    */
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

  test("unify bindings") {
    val $v = Var(-301)
    val $w = Var(-302)
    assert(Expr.unify(Expr(a, Expr(b, $x), Expr(f, $y, $x)),
                      Expr(a, Expr(b, $z), Expr(f, $z, Expr(g, $v, $w)))) ==
    Map($x.leftMost -> App(App(g, $v), $w),
        $y.leftMost -> App(App(g, $v), $w),
        $z.leftMost -> App(App(g, $v), $w)))

    try
      Expr.unifyTo(Expr(`=`, App(App(App(f, $), _1), $), _2), Expr(`=`, App(App($, $), $), $))
      assert(false)
    catch case Solver.Cycle(r, d) => ()

    try
      Expr.unifyTo(Expr(`=`, App(App(App(f, $), _1), $), _2), Expr(`=`, App(App(f, $), App(App(_1, $), $)), $))
      assert(false)
    catch case Solver.Conflict(_, _) => ()
  }

  test("unify multiple") {
    /*
    for all unifiable E1, E2, E3
    val m = Expr.unify(E1, E2, E3)
    E1.substAbs(m) == E2.substAbs(m) == E3.substAbs(m)
    */
    assert(Expr.unify(Expr($x, a, $x), $y, Expr(Expr(a, b), $z, Expr($z, b))) == Map(
      $x -> Expr(a, b),
      $y -> Expr(Expr(a, b), a, Expr(a, b)),
      $z -> a
    ).map{ case (Var(i), e) => i -> e })

    assert(Expr.unify(Expr(a, Expr(a, $x), Expr(a, $x, $y)), Expr($z, Expr($z, b), Expr($z, b, c))) == Map(
      $x -> b,
      $y -> c,
      $z -> a
    ).map{ case (Var(i), e) => i -> e })

    assert(Expr.unify(
      Expr(Expr(f, Var(-11)), Expr(Var(-12), Var(-13))),
      Expr(Expr(Var(-20), a), Expr(Var(-22), Var(-23))),
      Expr(Expr(Var(-30), Var(-31)), Expr(g, Var(-33))),
      Expr(Expr(Var(-40), Var(-41)), Expr(Var(-42), b))
    ) == Map(-22 -> g, -40 -> f, -11 -> a, -23 -> b, -30 -> f, -42 -> g, -20 -> f, -33 -> b, -31 -> a, -12 -> g, -13 -> b, -41 -> a))
  }

  test("transform") {
    assert(Expr(A, a, b).transform(Expr(A, $, $), Expr(B, _2, _1)) == Expr(B, b, a))

    val pair = Var(1000)
    val rightItem = Var(1001)
    val list = Var(1002)
    val head = Var(1003)
    val last = Var(1004)

    {
      val data =    Expr(pair, a, b)
      val pattern = Expr(pair, a, $)
      val template = Expr(rightItem, _1)
      assert(data.transform(pattern, template) == Expr(rightItem, b))      
    }
    
    {
      val listData = Expr(list, Expr(pair, a, b), Expr(pair, b, c), Expr(pair, A, A))
      val listOf3pattern = Expr(list, $, $, $)
      val headTemplate = Expr(head, _1)
      val lastTemplate = Expr(last, _3)

      val extremaTemplate = Expr(pair, _1, _3)

      assert(listData.transform(listOf3pattern, headTemplate) == Expr(head, Expr(pair, a, b)))
      assert(listData.transform(listOf3pattern, lastTemplate) == Expr(last, Expr(pair, A, A)))
      assert(listData.transform(listOf3pattern, extremaTemplate) == Expr(pair, Expr(pair, a, b), Expr(pair, A, A)))
    }
  }

  test("subst") {
    assert(e1.substRel(Seq(b)) == Expr(f, b, a))
    assert(e2.substRel(Seq(a, b)) == Expr(f, a, b, b, a))
    assert(e3.substRel(Seq(a, b)) == Expr(f, a, Expr(g, a, b)))
  }

  test("large subst") {
    import Expr.*
    val `5` = Var(200)
    val Z = Var(201)
    assert(r1.substRel(Seq(`5`, App(g,App(g,Z)))) == App(App(`=`,App(f,App(App(`,`,`5`),App(g,App(g,App(g,App(g,Z))))))),App(App(h,App(App(`,`,`5`),App(g,a))),App(App(`,`,`5`),App(g,App(g,App(g,Z)))))))
  }

  test("show pretty") {
    assert(e1.show == "Expr(Var(1), Var(0), Var(10))")
    assert(e2.show == "Expr(Var(1), Var(0), Var(0), Var(-2), Var(-1))")
    assert(e3.show == "Expr(Var(1), Var(0), Expr(Var(2), Var(-1), Var(0)))")
    assert(e1.pretty(false) == "(1 ◆ 10)")
    assert(e2.pretty(false) == "(1 ◆ ◆ ⏴₂ ⏴₁)")
    assert(e3.pretty(false) == "(1 ◆ (2 ⏴₁ ◆))")
  }
