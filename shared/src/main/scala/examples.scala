package be.adamv.cz2

object ExprExamples:
  import Expr.*
  val f: Expr = Var(1)
  val g: Expr = Var(2)
  val h: Expr = Var(3)

  val a: Expr = Var(10)
  val b: Expr = Var(11)
  val c: Expr = Var(12)

  val A: Expr = Var(20)
  val B: Expr = Var(21)
  val C: Expr = Var(22)

  val `=`: Expr = Var(100)
  val `,`: Expr = Var(101)
  val `:`: Expr = Var(102)
  val `-->`: Expr = Var(103)
  val `*`: Expr = Var(104)
  val `+`: Expr = Var(105)

  val _1: Expr = Var(-1)
  val _2: Expr = Var(-2)
  val _3: Expr = Var(-3)
  val _4: Expr = Var(-4)
  val _5: Expr = Var(-5)

  val $: Expr = Var(0)

  val $x: Expr = Var(-100)
  val $y: Expr = Var(-200)
  val $z: Expr = Var(-300)

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


object ExprMapExamples:
  import ExprExamples.*

  val sharing = ExprMap[Int]()
  sharing.update(Expr(`=`, a, b), 1)
  sharing.update(Expr(`=`, Expr(f, $), Expr(a, _1)), 2)
  sharing.update(Expr(`=`, Expr(f, b), c), 3)
  // Expr(`=`, Expr($, $), $) ==> Expr(`,`, _1, _3)
  sharing.update(Expr(`=`, Expr(g, $, $), Expr(a, Expr(c, _1), _2)), 4)

  val inner = ExprMap[Int]()
  inner.update(Expr(a, b, c, f), 1)
  inner.update(Expr(a, b, c, g), 2)
  inner.update(Expr(a, b, c, h), 3)

  val bidi = ExprMap[Int]()
  bidi.update(Expr(`:`, f, Expr(-->, A, A)), 10)
  bidi.update(Expr(`=`, Expr(f, $), Expr(a, _1)), 11)
  bidi.update(Expr(`:`, g, Expr(-->, A, A)), 20)
  bidi.update(Expr(`=`, Expr(g, Expr(a, $)), _1), 21)
  bidi.update(Expr(`:`, h, Expr(-->, Expr(`,`, A, A), A)), 30)
  bidi.update(Expr(`=`, Expr(h, Expr(`,`, b, $)), _1), 31)
  bidi.update(Expr(`=`, Expr(h, Expr(`,`, Expr(a, $), $)), Expr(a, Expr(h, Expr(`,`, _1, _2)))), 32)

  val prob = ExprMap[Int]()
  prob.update(Expr(`=`, Expr(f, $, $), _1), 20)
  prob.update(Expr(`=`, Expr(f, $, $), _2), 21)
  prob.update(Expr(`=`, Expr(g, Expr(h, $)), _1), 30)

  prob.update(Expr(`=`, A, Expr(h, Expr(f, Var(1000), Var(1001)))), 40)

  prob.update(Expr(`=`, B, Expr(h, Expr(f, Var(1010), Expr(f, Var(1011), Var(1012))))), 50)

  prob.update(Expr(`=`, Expr(Var(1020), Var(1000)), Expr(g, A)), 60)
  prob.update(Expr(`=`, Expr(Var(1020), Var(1001)), Expr(g, B)), 61)
  prob.update(Expr(`=`, C, Expr(h, Expr(Var(1020), Expr(g, A)))), 70)

  val partialf = ExprMap[Int]()
  partialf.update(Expr(`=`, A, B), 10)
  partialf.update(Expr(`=`, A, C), 11)

  partialf.update(Expr(`=`, Expr(f, B), b), 20)

  val firstclass = ExprMap[Int]()
  firstclass.update(Expr(`=`, Expr(f, $), Expr(Expr(g, _1), Expr(h, _1))), 10)

  firstclass.update(Expr(`=`, Expr(g, a), A), 20)
  firstclass.update(Expr(`=`, Expr(g, b), A), 21)
  firstclass.update(Expr(`=`, Expr(g, c), B), 22)

  firstclass.update(Expr(`=`, Expr(h, a), c), 30)
  firstclass.update(Expr(`=`, Expr(h, b), b), 31)
  firstclass.update(Expr(`=`, Expr(h, c), a), 32)

  firstclass.update(Expr(`=`, Expr(A, a), b), 40)
  firstclass.update(Expr(`=`, Expr(A, b), c), 41)
  firstclass.update(Expr(`=`, Expr(A, c), a), 42)

  firstclass.update(Expr(`=`, Expr(B, $), _1), 50)

  val simplelinear = ExprMap[Int]()
  simplelinear.update(Expr(`=`, A, B), 10)

  simplelinear.update(Expr(`=`, Expr(f, $), Expr(a, _1)), 30)

  simplelinear.update(Expr(`=`, Expr(g, Expr(b, $)), _1), 30)

  val variety = ExprMap[Int]()
  variety.update(Expr(Var(1000), Var(1001), Var(1002), Var(1003)), 0)

  variety.update(Expr.nest(C, B, B, A), 10)
  variety.update(Expr.nest(C, B, B, B, A), 11)

  variety.update(Expr(a, $, $), 20)
  variety.update(Expr(b, $, A), 21)
  variety.update(Expr(c, A, $), 22)
  
  variety.update(Expr(Var(2000), Expr.nest(B, B, A), Expr.nest(B, A)), 30)
  variety.update(Expr(Var(2001), Expr(B, A), Expr.nest(B, B, A)), 31)
  variety.update(Expr(Var(2002), A, Expr.nest(B, B, B, B, A)), 32)
  
  variety.update(Expr(`=`, Expr(`+`, $, A), _1), 40)
  variety.update(Expr(`=`, Expr(`+`, $, Expr(B, $)), Expr(B, Expr(`+`, _1, _2))), 41)
  variety.update(Expr(`=`, Expr(`*`, $, A), A), 42)
  variety.update(Expr(`=`, Expr(`*`, $, Expr(B, $)), Expr(`+`, _1, Expr(`*`, _1, _2))), 43)
