package be.adamv.cz2

import munit.FunSuite


class EquationsTransform extends FunSuite:
  import ExprExamples.{`=`, $, _1, _2, _3, $x, $y, $z}

  val Vars(prob, when,
           compliment, union, intersection,
           div, mul, add, sub, one,
           equal, variations, equiv, transform, _*) = RangeStorage.highPos[Int]()

  // plumbing
  val pfs = collection.mutable.Map.empty[Int, ExprMap[_] => ExprMap[_]]
  var pc = 10000

  given [A]: PartialFunction[Int, ExprMap[A] => ExprMap[A]] = {
    case i if i == transform.leftMost => em =>
      ExprMap.from(em.items.map((e1, s1) =>
        pc += 1
        pfs(pc) = em2 => ExprMap.from(em2.items.flatMap((e2, s2) =>
          space.transform(e1, e2).items
        ))
        Expr.Var(pc) -> s1
      ))
    case pfs(handler) => handler.asInstanceOf[ExprMap[A] => ExprMap[A]]
  }


  given space: ExprMap[Int] = ExprMap(
    Expr(equal,
      Expr(prob, Expr(compliment, $x)),
      Expr(sub, one, Expr(prob, $x))
    ) -> 1,
    Expr(equal,
      Expr(prob, Expr(compliment, $y), when, $x),
      Expr(sub, one, Expr(prob, $y, when, $x)),
    ) -> 2,
    Expr(equal,
      Expr(prob, $x, when, $y),
      Expr(div, Expr(prob, Expr(union, $x, $y)), Expr(prob, $x)),
      ) -> 3,
    Expr(equal,
      Expr(prob, $x),
      Expr(add,
        Expr(mul, Expr(prob, $x, when, $y), Expr(prob, $y)),
        Expr(mul, Expr(prob, $x, when, Expr(compliment, $y)), Expr(prob, Expr(compliment, $y)))),
    ) -> 4,

    Expr(equiv, Expr(equal, $, $), Expr(equal, _2, _1)) -> 10,

    Expr(equiv, Expr(equal, $, Expr(sub, $, $)), Expr(equal, Expr(add, _1, _3), _2)) -> 20,

    Expr(equiv, Expr(equal, $, Expr(div, $, $)), Expr(equal, Expr(mul, _1, _3), _2)) -> 30,

    Expr(equiv, Expr(union, $, $),  Expr(union, _2, _1)) -> 40,

    Expr(equiv, Expr(add, $, $),  Expr(add, _2, _1)) -> 50,

    Expr(equiv, Expr(mul, $, $),  Expr(mul, _2, _1)) -> 60,

    Expr(`=`, Expr(variations, $), _1) -> 100,
    Expr(`=`, Expr(variations, $), Expr(transform, Expr(equiv, _1, $), _2)) -> 101,
    Expr(`=`, Expr(variations, $), Expr(transform, Expr(equiv, $, _1), _2)) -> 102,
  )

  test("variations") {
    import ValueEvaluationAlgorithms.ignore

    assert(ignore[Int].evalGroundedN(Expr(variations,
      Expr(equal, Expr(prob, Expr(compliment, $)), Expr(sub, one, Expr(prob, _1)))
    ), -1, 20).keys.toSet == Set(
      Expr(equal, Expr(prob, Expr(compliment, $)), Expr(sub, one, Expr(prob, _1))),
      Expr(equal, Expr(sub, one, Expr(prob, $)), Expr(prob, Expr(compliment, _1))),
      Expr(equal, Expr(add, Expr(prob, Expr(compliment, $)), Expr(prob, _1)), one)
    ))
  }
