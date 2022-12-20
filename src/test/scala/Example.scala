package be.adamv.cz2

import munit.FunSuite


class RecursionSchemeTest extends FunSuite:
  import ExprExamples.{`=`, _1, _2, _3, $}
  val S = Var(1)
  val Z = Var(2)

  val Fix = Var(10)
  val unFix = Var(11)

  val cata = Var(20)

  val mapNat = Var(30)

  val wrap = Var(40)
  val double = Var(41)

  val base = ExprMap(
    Expr(`=`, Expr(unFix, Expr(Fix, $)), _1) -> "unFix",
//                      F  alg  a
    Expr(`=`, Expr(cata, $, $, $), Expr(_2, Expr(_1, Expr(cata, _1, _2), Expr(unFix, _3)))) -> "cata",

    Expr(`=`, Expr(mapNat, $, Expr(S, $)), Expr(S, Expr(_1, _2))) -> "mapNatS",
    Expr(`=`, Expr(mapNat, $, Z), Z) -> "mapNatZ",

    Expr(`=`, Expr(wrap, Expr(S, $)), Expr.nest(Fix, S, _1)) -> "wrapS",
    Expr(`=`, Expr(wrap, Z), Expr.nest(Fix, S, Fix, Z)) -> "wrapZ",

    Expr(`=`, Expr(double, Expr(S, $)), Expr.nest(Fix, S, Fix, S, _1)) -> "doubleS",
    Expr(`=`, Expr(double, Z), Expr(Fix, Z)) -> "doubleZ",
  )

  def intToNat(i: Int): Expr = if i == 0
    then Expr.App(Fix, Z)
    else Expr.App(Fix, Expr.App(S, intToNat(i - 1)))

  test("eval") {
    import EvaluationAlgorithms.*

    given ExprMap[String] = base
    val k = 5
    assert(eval(Expr(cata, mapNat, wrap, intToNat(k))) == intToNat(k + 1))
    assert(eval(Expr(cata, mapNat, double, intToNat(k))) == intToNat(2*k))
  }
