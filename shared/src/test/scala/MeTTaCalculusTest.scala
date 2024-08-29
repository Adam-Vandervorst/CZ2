package be.adamv.cz2

import munit.FunSuite


//noinspection NonAsciiCharacters
class MeTTaCalculusTest extends FunSuite:
  import ExprExamples.{`=`, $, _1, _2, _3, $x, $y, $z}

  val Vars(succ, s, z, output, _*) = RangeStorage.highPos[Int]()

  given executor: MeTTaCalculus = MeTTaCalculus()
  import MeTTaCalculus.{==>, ∅}

  test("basic") {
    val succf = Expr(Expr(succ, $, $), ==>, Expr(_2, ==>, Expr(s, _1)))
    val App(App(succf_lhs, `==>`), succf_rhs) = succf
    val succc = Expr(Expr(succ, Expr(s, Expr(s, z)), output), ==>, ∅)
    val App(App(succc_lhs, `==>`), succc_rhs) = succc
    val Some(lhsb, rhsb) = succf_lhs.matches(succc_lhs)
    assert(succf_rhs.substReIndex(lhsb) == Expr(output, ==>, Expr(s, Expr(s, Expr(s, z)))))
    assert(succc_rhs.substReIndex(rhsb) == ∅)
  }
