package be.adamv.cz2

import munit.FunSuite


class EquationsTransform extends FunSuite:
  import ExprExamples.{`=`, $, _1, _2, _3, $x, $y, $z}

  // plumbing
  val pfs = collection.mutable.Map.empty[Int, ExprMap[_] => ExprMap[_]]
  var pc = 10000

  given [A]: PartialFunction[Int, ExprMap[A] => ExprMap[A]] = {
    case 2002 => em =>
      ExprMap.from(em.items.map((e1, s1) =>
        pc += 1
        pfs(pc) = em2 => ExprMap.from(em2.items.flatMap((e2, s2) =>
          space.transform(e1, e2).items
        ))
        Expr.Var(pc) -> s1
      ))
    case pfs(handler) => handler.asInstanceOf[ExprMap[A] => ExprMap[A]]
  }

  // prob
  val P = Expr.Var(1000)
  val | = Expr.Var(1001)

  // set
  val ~ = Expr.Var(1010)
  val Union = Expr.Var(1011)
  val Intersection = Expr.Var(1012)

  // real
  val / = Expr.Var(1020)
  val * = Expr.Var(1021)
  val + = Expr.Var(1022)
  val - = Expr.Var(1023)
  val `1` = Expr.Var(1024)

  // equation
  val equates = Expr.Var(1030)

  val variations = Expr.Var(2000)
  val equiv = Expr.Var(2001)
  val transform = Expr.Var(2002)

  given space: ExprMap[Int] = ExprMap(
    Expr(equates,
      Expr(P, Expr(~, $x)),
      Expr(-, `1`, Expr(P, $x))
    ) -> 1,
    Expr(equates,
      Expr(P, Expr(~, $y), |, $x),
      Expr(-, `1`, Expr(P, $y, |, $x)),
    ) -> 2,
    Expr(equates,
      Expr(P, $x, |, $y),
      Expr(/, Expr(P, Expr(Union, $x, $y)), Expr(P, $x)),
      ) -> 3,
    Expr(equates,
      Expr(P, $x),
      Expr(+,
        Expr(*, Expr(P, $x, |, $y), Expr(P, $y)),
        Expr(*, Expr(P, $x, |, Expr(~, $y)), Expr(P, Expr(~, $y)))),
    ) -> 4,

    Expr(equiv, Expr(equates, $, $), Expr(equates, _2, _1)) -> 10,

    Expr(equiv, Expr(equates, $, Expr(-, $, $)), Expr(equates, Expr(+, _1, _3), _2)) -> 20,

    Expr(equiv, Expr(equates, $, Expr(/, $, $)), Expr(equates, Expr(*, _1, _3), _2)) -> 30,

    Expr(equiv, Expr(Union, $, $),  Expr(_2, _1)) -> 40,

    Expr(equiv, Expr(+, $, $),  Expr(_2, _1)) -> 50,

    Expr(equiv, Expr(*, $, $),  Expr(_2, _1)) -> 60,

    Expr(`=`, Expr(variations, $), _1) -> 100,
    Expr(`=`, Expr(variations, $), Expr(transform, Expr(equiv, _1, $), _2)) -> 101,
    Expr(`=`, Expr(variations, $), Expr(transform, Expr(equiv, $, _1), _2)) -> 102,
  )

  test("variations") {
    import ValueEvaluationAlgorithms.ignore

    assert(ignore[Int].evalGroundedN(Expr(variations,
      Expr(equates, Expr(P, Expr(~, $)), Expr(-, `1`, Expr(P, _1)))
    ), -1, 20).keys.toSet == Set(
      Expr(equates, Expr(P, Expr(~, $)), Expr(-, `1`, Expr(P, _1))),
      Expr(equates, Expr(-, `1`, Expr(P, $)), Expr(P, Expr(~, _1))),
      Expr(equates, Expr(+, Expr(P, Expr(~, $)), Expr(P, _1)), `1`),
      Expr(Union, Expr(-, `1`, Expr(P, $)), Expr(equates, Expr(P, Expr(~, _1)))),
      Expr(+, Expr(-, `1`, Expr(P, $)), Expr(equates, Expr(P, Expr(~, _1)))),
      Expr(*, Expr(-, `1`, Expr(P, $)), Expr(equates, Expr(P, Expr(~, _1))))
    ))
  }
