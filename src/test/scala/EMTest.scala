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
    def evalDirect(e: Expr): ExprMap[Int] =
      bidi.transform(Expr(`=`, e, $), _1)

    def evalBottomUp(e: Expr): ExprMap[Int] =
      e.foldMap(i => ExprMap(Var(i)), ???)

    var ev = 0
    def leaves(seed: Expr, options: Expr => ExprMap[Int]): ExprMap[Int] =
      val next = options(seed)
      if next.isEmpty then ExprMap(seed -> {ev += 1; ev}) else ExprMap.from(next.items.flatMap((x, v) => leaves(x, options).items))

//    def breadth[A](pop: Set[A], options: A => Set[A], last: Option[Set[A]] = None): Set[A] =
//      if last.nonEmpty && last.get == pop then pop
//      else breadth(pop | pop.flatMap(options), options, Some(pop))


    assert(evalDirect(Expr(f, b)).keys.toSet == Set(Expr(a, b)))
    assert(evalDirect(Expr(Var(42), b)).keys.isEmpty)
    assert(evalDirect(Expr(h, Expr(`,`, Expr(a, Expr(a, b)), Expr(a, Expr(a, b))))).keys.toSet ==
           Set(Expr(a, Expr(h, Expr(`,`, Expr(a, b), Expr(a, Expr(a, b)))))))

    println(leaves(Expr(h, Expr(`,`, Expr(a, Expr(a, b)), Expr(a, Expr(a, b)))), evalDirect).keys.map(_.pretty).mkString("\n"))
  }