package be.adamv

import munit.FunSuite

object ExprMapExamples:
  import ExprExamples.*

  val sharing = ExprMap[Int]()
  sharing.update(Expr(`=`, a, b), 1)
  sharing.update(Expr(`=`, Expr(f, $), Expr(a, _1)), 2)
  sharing.update(Expr(`=`, Expr(f, b), c), 3)
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


object EvaluationAlgorithms:
  import ExprExamples.*

  def evalDirect(e: Expr)(using s: ExprMap[_]): Set[Expr] =
    s.transform(Expr(`=`, e, $), _1).keys.toSet

  def maybeEval(e: Expr)(using s: ExprMap[_]): Set[Expr] =
    val nv = evalDirect(e)
    if nv.isEmpty then Set(e)
    else nv

  def evalBottomUp(e: Expr)(using s: ExprMap[_]): Set[Expr] =
    e.foldMap(i => maybeEval(Var(i)), (fem, aem) => fem.flatMap(f => aem.flatMap(a => maybeEval(App(f, a)))))

  def allpossible(e: Expr)(using s: ExprMap[_]): Set[Expr] =
    fix[Set[Expr]](_.flatMap(evalBottomUp))(Set(e))

abstract class ValueEvaluationAlgorithms[V]:
  import ExprExamples.*

  def lookup(emv: V, ev: V): V
  def merge(fv: V, av: V): V

  def evalDirect(e: Expr, v: V)(using s: ExprMap[V]): ExprMap[V] =
    s.transform(Expr(`=`, e, $), _1).map(w => lookup(w, v))

  def maybeEval(e: Expr, v: V)(using s: ExprMap[V]): ExprMap[V] =
    val nv = evalDirect(e, v)
    if nv.isEmpty then ExprMap(e -> v)
    else nv

  def evalBottomUp(e: Expr, v: V)(using s: ExprMap[V]): ExprMap[V] =
    e.foldMap(i => maybeEval(Var(i), v), (fem, aem) => ExprMap.from(fem.items.flatMap((f, fv) => aem.items.flatMap((a, av) => maybeEval(App(f, a), merge(fv, av)).items))))

  def allpossible(e: Expr, v: V)(using s: ExprMap[V]): ExprMap[V] =
    fixproject[ExprMap[V], Set[Expr]](em => ExprMap.from(em.items.flatMap(evalBottomUp(_, _).items)), _.keys.toSet)(ExprMap[V](e -> v))

object ValueEvaluationAlgorithms:
  val graphvizDebug: ValueEvaluationAlgorithms[Int] = new:
    def lookup(emv: Int, ev: Int): Int =
      val r = 100 + util.Random.nextInt(9900)
      println(s"$r [label=\"lookup $emv\"]")
      println(s"$ev -> $r [label=$ev]")
      //        println(s"lookup  ${emv.toString.padTo(10, ' ')} ${ev.toString.padTo(10, ' ')} ${r.toString.padTo(10, ' ')}")
      r

    def merge(fv: Int, av: Int): Int =
      val r = 100 + util.Random.nextInt(9900)
      println(s"$r [label=\"merge\"]")
      println(s"$fv -> $r [label=$fv]")
      println(s"$av -> $r [label=$av]")
      //        println(s"merge   ${fv.toString.padTo(10, ' ')} ${av.toString.padTo(10, ' ')} ${r.toString.padTo(10, ' ')}")
      r

class ExprMapTest extends FunSuite:
  import ExprExamples.*
  import ExprMapExamples.*

  test("EmptyEM contains get keys") {
    assert(!ExprMap().contains(f))
    assert(!ExprMap().contains(e1))

    assert(ExprMap().get(f).isEmpty)
    assert(ExprMap().get(e1).isEmpty)

    assert(ExprMap().keys.isEmpty)
  }

  test("EmptyEM update get") {
    assert(ExprMap(f -> ()).get(f).contains(()))
    assert(ExprMap(e1 -> ()).get(e1).contains(()))
  }

  test("EM orthogonal contains get keys") {
    val nem = ExprMap(f -> 1, e1 -> 2)
    assert(nem.contains(f))
    assert(nem.contains(e1))
    assert(nem.get(f).contains(1))
    assert(nem.get(e1).contains(2))
    assert(nem.keys.toSet == Set(f, e1))
  }

  test("EM overlapping get keys") {
    val nem = ExprMap(e1 -> 1, e2 -> 2, e3 -> 3)
    assert(nem.contains(e1))
    assert(nem.contains(e2))
    assert(nem.contains(e3))
    assert(nem.get(e1).contains(1))
    assert(nem.get(e2).contains(2))
    assert(nem.get(e3).contains(3))
    assert(nem.keys.toSet == Set(e1, e2, e3))
  }

  test("many updates size") {
    val em = ExprMap[Int]()
    val number = Expr.Var(1)

    for i <- 0 until 1_000_000 do
      em.update(Expr.App(number, Expr.Var(i)), i)

    assert(em.get(Expr.App(number, Expr.Var(666_666))).contains(666_666))
    assert(em.size == 1_000_000)
  }

  test("ExprMap deep updates size") {
    val em = ExprMap[Int]()
    val F = Expr.Var(1)

    def wrap(e: Expr, depth: Int): Expr =
      if depth == 0 then e else wrap(Expr.App(F, e), depth - 1)

    for i <- 0 until 1_000 do
      em.update(wrap(Expr.Var(i), 1000), i)

    assert(em.get(wrap(Expr.Var(666), 1000)).contains(666))
    assert(em.size == 1000)
  }

  test("ExprMap JSON") {
    assert(sharing.json == """{"@":{"@":{"100":{"10":{"11":1},"@":{"1":{"0":{"@":{"10":{"-1":2}}},"11":{"12":3}},"@":{"2":{"0":{"0":{"@":{"@":{"10":{"@":{"12":{"-1":{"-2":4}}}}}}}}}}}}}}}""")
    assert(inner.json == """{"@":{"@":{"@":{"10":{"11":{"12":{"1":1,"2":2,"3":3}}}}}}}""")
  }

  test("ExprMap pretty") {
    assert(ExprMap(a -> 1, b -> 2).pretty(colored=false) == eids"⧼$a: 1, $b: 2⦒")
    assert(ExprMap(a -> 1, Expr(f, b) -> 2).pretty(colored=false) == eids"⧼$a: 1|⧼$f: ⧼$b: 2⦒⦒⧽")

    assert(sharing.pretty(colored=false) == eids"⦑⦑⧼${`=`}: ⧼$a: ⧼$b: 1⦒|⧼1: ⧼◆: ⦑⧼$a: ⧼⏴₁: 2⦒⦒⧽, $b: ⧼$c: 3⦒⦒|⧼$g: ⧼◆: ⧼◆: ⦑⦑⧼$a: ⦑⧼$c: ⧼⏴₁: ⧼⏴₂: 4⦒⦒⦒⧽⦒⧽⧽⦒⦒⦒⧽⧽⦒⧽⧽")
    assert(inner.pretty(colored=false) == eids"⦑⦑⦑⧼$a: ⧼$b: ⧼$c: ⧼$f: 1, $g: 2, $h: 3⦒⦒⦒⦒⧽⧽⧽")
  }

  test("indiscriminateMatching") {
    assert(sharing.indiscriminateMatching(Expr(`=`, Expr(_1, _2), _3)).values.toSet == Set(2, 3, 4))
    assert(inner.indiscriminateMatching(Expr(a, b, $, f)).values.toSet == Set(1))
  }

  test("indiscriminateMatching keys items getUnsafe") {
    assert(sharing.indiscriminateMatching($).items == sharing.items)
    assert(inner.indiscriminateMatching($).items == inner.items)

    assert(sharing.keys.forall(k => sharing.indiscriminateMatching(k).items.head == k -> sharing.getUnsafe(k)))
    assert(inner.keys.forall(k => inner.indiscriminateMatching(k).items.head == k -> inner.getUnsafe(k)))
  }

  test("indiscriminateReverseMatching") {
    assert(sharing.indiscriminateReverseMatching(Expr(`=`, Expr(f, a), Expr(a, f))).values.toSet == Set(2))
    assert(inner.indiscriminateReverseMatching(Expr(a, b, c, f)).values.toSet == Set(1))
    assert(bidi.indiscriminateReverseMatching(Expr(`=`, Expr(f, b), Expr(a, b))).values.toSet == Set(11))
  }

  test("indiscriminateBidirectionalMatching") {
    assert(bidi.indiscriminateBidirectionalMatching(Expr(`:`, $, Expr(-->, $, $))).values.toSet == Set(10, 20, 30))
    assert(bidi.indiscriminateBidirectionalMatching(Expr(`=`, Expr(f, Expr(a, Expr(a, b))), $)).values.toSet == Set(11))
  }

  test("transform") {
    assert(sharing.transform(Expr(`=`, Expr($, $), $), Expr(`,`, _1, _3)).keys.toSet ==
           Set(Expr(`,`, f, c), Expr(`,`, f, Expr(a, _1)), Expr(`,`, Expr(g, _1), Expr(a, Expr(c, _1), _2))))

    val endo = Var(170)
    assert(bidi.transform(Expr(`:`, $, Expr(-->, $, _2)), Expr(endo, _1, _2)).keys.toSet ==
           Set(Expr(endo, f, A), Expr(endo, g, A)))
  }

  test("evaluation") {
    import EvaluationAlgorithms.*
    {
      given ExprMap[Int] = bidi
      assert(evalDirect(Expr(f, b)) == Set(Expr(a, b)))
      assert(evalDirect(Expr(Var(42), b)).isEmpty)
      assert(evalDirect(Expr(h, Expr(`,`, Expr(a, Expr(a, b)), Expr(a, Expr(a, b))))) ==
             Set(Expr(a, Expr(h, Expr(`,`, Expr(a, b), Expr(a, Expr(a, b)))))))
      assert(allpossible(Expr(h, Expr(`,`, Expr(a, Expr(a, b)), Expr(a, Expr(a, b))))) ==
             Set(Expr(a, Expr(a, Expr(a, Expr(a, b))))))
    }
    {
      given ExprMap[Int] = prob
      assert(allpossible(Expr(g, A)) == Set(Var(1000), Var(1001)))
      assert(allpossible(Expr(g, B)) == Set(Var(1010), Var(1011), Var(1012)))
      assert(allpossible(Expr(g, C)) == Set(Var(1000), Var(1001), Var(1010), Var(1011), Var(1012)))
    }
    {
      given ExprMap[Int] = partialf

      assert(allpossible(Expr(f, A)) == Set(Expr(f, C), b))
    }
  }

  test("traced evaluation") {


    import ValueEvaluationAlgorithms.graphvizDebug.*
    val base = 10000

//        {
//      given ExprMap[Int] = bidi

//      assert(evalDirect(Expr(f, b), base).items.toSet ==
//             Set((Expr(a, b), Expr(Var(11), base))))
//      assert(evalDirect(Expr(Var(42), b), base).isEmpty)
//      println(allpossible(Expr(h, Expr(`,`, Expr(a, Expr(a, b)), Expr(a, Expr(a, b)))), base).items.map((x, y) => (x.pretty, y.toString)).mkString("\n"))
//      assert(allpossible(Expr(h, Expr(`,`, Expr(a, Expr(a, b)), Expr(a, Expr(a, b)))), base).items.toSet ==
//             Set((Expr(a, Expr(a, Expr(a, Expr(a, b)))), Expr(Var(31), Expr(Var(32), Expr(Var(32), base))))))
//
//    }
    {
      given ExprMap[Int] = prob
      println(allpossible(Expr(g, C), base).items.map((x, y) => (x.pretty, y.toString)).mkString("\n"))
    }
//      assert(allpossible(Expr(g, A), base).items.toSet ==
//        Set((Var(1000), Expr.nest(Var(20), Var(30), Var(40), base)),
//            (Var(1001), Expr.nest(Var(21), Var(30), Var(40), base))))
//      assert(allpossible(Expr(g, B), base).items.toSet ==
//        Set((Var(1010), Expr.nest(Var(20), Var(21), Var(30), Var(50), base)),
//            (Var(1011), Expr.nest(Var(21), Var(20), Var(30), Var(50), base)),
//            (Var(1012), Expr.nest(Var(21), Var(21), Var(30), Var(50), base))))
//      assert(allpossible(Expr(g, C), base).items.toSet ==
//        Set((Var(1000), Expr.nest(Var(20), Var(30), Var(40), Var(60), Var(20), Var(30), Var(40), Var(30), Var(70), base)),
//            (Var(1001), Expr.nest(Var(21), Var(30), Var(40), Var(60), Var(20), Var(30), Var(40), Var(30), Var(70), base)),
//            (Var(1010), Expr.nest(Var(20), Var(21), Var(30), Var(50), Var(61), Var(21), Var(30), Var(40), Var(30), Var(70), base)),
//            (Var(1011), Expr.nest(Var(21), Var(20), Var(30), Var(50), Var(61), Var(21), Var(30), Var(40), Var(30), Var(70), base)),
//            (Var(1012), Expr.nest(Var(21), Var(21), Var(30), Var(50), Var(61), Var(21), Var(30), Var(40), Var(30), Var(70), base))))
//    }
//    {
//      given ExprMap[(Int, Set[Int])] = firstclass.map(i => (i, Set()))
//      println(allpossible(Expr(f, a), base).items.map((x, y) => (x.pretty, y.toString)).mkString("\n"))
//      println()
//      println(allpossible(Expr(f, b), base).items.map((x, y) => (x.pretty, y.pretty)).mkString("\n"))
//      println()
//      println(allpossible(Expr(f, c), base).items.map((x, y) => (x.pretty, y.pretty)).mkString("\n"))
//    }
  }

