package be.adamv.cz2

import munit.FunSuite


class RecursionSchemeTest extends FunSuite:
  import ExprExamples.{`=`, _1, _2, _3, $}
  val S = Var(1)
  val Z = Var(2)
  val Cons = Var(3)
  val Nil = Var(4)

  val Fix = Var(10)
  val unFix = Var(11)

  val cata = Var(20)
  val ana = Var(21)
  val hylo = Var(22)

  val mapNat = Var(30)
  val mapList = Var(31)

  val wrap = Var(40)
  val double = Var(41)

  val iota = Var(50)
  val length = Var(51)

  val base = ExprMap(
    Expr(`=`, Expr(unFix, Expr(Fix, $)), _1) -> "unFix",

    Expr(`=`, Expr(cata, $, $, $), Expr(_2, Expr(_1, Expr(cata, _1, _2), Expr(unFix, _3)))) -> "cata",
    Expr(`=`, Expr(ana, $, $, $), Expr(Fix, Expr(_1, Expr(ana, _1, _2), Expr(_2, _3)))) -> "ana",

    Expr(`=`, Expr(mapNat, $, Expr(S, $)), Expr(S, Expr(_1, _2))) -> "mapNatS",
    Expr(`=`, Expr(mapNat, $, Z), Z) -> "mapNatZ",

    Expr(`=`, Expr(mapList, $, Expr(Cons, $, $)), Expr(Cons, _2, Expr(_1, _3))) -> "mapListS",
    Expr(`=`, Expr(mapList, $, Nil), Nil) -> "mapListZ",

    Expr(`=`, Expr(wrap, Expr(S, $)), Expr.nest(Fix, S, _1)) -> "wrapS",
    Expr(`=`, Expr(wrap, Z), Expr.nest(Fix, S, Fix, Z)) -> "wrapZ",

    Expr(`=`, Expr(double, Expr(S, $)), Expr.nest(Fix, S, Fix, S, _1)) -> "doubleS",
    Expr(`=`, Expr(double, Z), Expr(Fix, Z)) -> "doubleZ",

    Expr(`=`, Expr(iota, Expr(Fix, Expr(S, $))), Expr(Cons, _1, _1)) -> "iotaS",
    Expr(`=`, Expr(iota, Expr(Fix, Z)), Nil) -> "iotaZ",

    Expr(`=`, Expr(length, Expr(Cons, $, $)), Expr.nest(Fix, S, _2)) -> "lengthCons",
    Expr(`=`, Expr(length, Nil), Expr(Fix, Z)) -> "lengthNil",
  )

  def intToNat(i: Int): Expr = if i == 0
    then Expr.App(Fix, Z)
    else Expr.App(Fix, Expr.App(S, intToNat(i - 1)))

  def seqToList(s: Seq[Expr]): Expr = if s.isEmpty
    then Expr.App(Fix, Nil)
    else Expr.App(Fix, Expr(Cons, s.head, seqToList(s.tail)))

  test("eval") {
    import EvaluationAlgorithms.*

    given ExprMap[String] = base
    val k = 5
    assert(eval(Expr(cata, mapNat, wrap, intToNat(k))) == intToNat(k + 1))
    assert(eval(Expr(cata, mapNat, double, intToNat(k))) == intToNat(2*k))

    assert(eval(Expr(ana, mapList, iota, intToNat(k))) == seqToList((0 until k).reverse.map(intToNat)))
    assert(eval(Expr(cata, mapList, length, seqToList((0 until k).map(_ => Nil)))) == intToNat(k))
  }
