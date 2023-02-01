package be.adamv.cz2

import munit.FunSuite

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

  test("ExprMap prettyStructured") {
    assert(ExprMap(a -> 1, b -> 2).prettyStructured(colored=false) == eids"⧼$a: 1, $b: 2⦒")
    assert(ExprMap(a -> 1, Expr(f, b) -> 2).prettyStructured(colored=false) == eids"⧼$a: 1|⧼$f: ⧼$b: 2⦒⦒⧽")

    assert(sharing.prettyStructured(colored=false) == eids"⦑⦑⧼${`=`}: ⧼$a: ⧼$b: 1⦒|⧼1: ⧼◆: ⦑⧼$a: ⧼⏴₁: 2⦒⦒⧽, $b: ⧼$c: 3⦒⦒|⧼$g: ⧼◆: ⧼◆: ⦑⦑⧼$a: ⦑⧼$c: ⧼⏴₁: ⧼⏴₂: 4⦒⦒⦒⧽⦒⧽⧽⦒⦒⦒⧽⧽⦒⧽⧽")
    assert(inner.prettyStructured(colored=false) == eids"⦑⦑⦑⧼$a: ⧼$b: ⧼$c: ⧼$f: 1, $g: 2, $h: 3⦒⦒⦒⦒⧽⧽⧽")
  }

  test("ExprMap prettyStructuredSet") {
    assert(ExprMap(a -> 1, b -> 2).prettyStructuredSet(colored = false) == eids"⧼$a, $b⦒")
    assert(ExprMap(a -> 1, Expr(f, b) -> 2).prettyStructuredSet(colored = false) == eids"⧼$a|⧼$f: ⧼$b⦒⦒⧽")

    assert(sharing.prettyStructuredSet(colored = false) == eids"⦑⦑⧼${`=`}: ⧼$a: ⧼$b⦒|⧼$f: ⧼◆: ⦑⧼$a: ⧼⏴₁⦒⦒⧽, $b: ⧼$c⦒⦒|⧼$g: ⧼◆: ⧼◆: ⦑⦑⧼$a: ⦑⧼$c: ⧼⏴₁: ⧼⏴₂⦒⦒⦒⧽⦒⧽⧽⦒⦒⦒⧽⧽⦒⧽⧽")
    assert(inner.prettyStructuredSet(colored = false) == eids"⦑⦑⦑⧼$a: ⧼$b: ⧼$c: ⧼$f, $g, $h⦒⦒⦒⦒⧽⧽⧽")
  }

  test("ExprMap prettyListing") {
    assert(ExprMap(a -> 1, b -> 2).prettyListing(colored = false) == eids"$a\n$b")
    assert(ExprMap(a -> 1, Expr(f, b) -> 2).prettyStructuredSet(colored = false) == eids"⧼$a|⧼$f: ⧼$b⦒⦒⧽")

    assert(sharing.prettyListing(colored = false) == eids"(${`=`} $a $b)\n(${`=`} (1 ◆) ($a ⏴₁))\n(${`=`} ($f $b) $c)\n(${`=`} ($g ◆ ◆) ($a ($c ⏴₁) ⏴₂))")
    assert(inner.prettyListing(colored = false) == eids"($a $b $c $f)\n($a $b $c $g)\n($a $b $c $h)")
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
           Set(Expr(`,`, f, c),
             Expr(`,`, f, Expr(a, $)),
             Expr(`,`, Expr(g, $), Expr(a, Expr(c, _1), $))))

    val endo = Var(170)
    assert(bidi.transform(Expr(`:`, $, Expr(-->, $, _2)), Expr(endo, _1, _2)).keys.toSet ==
           Set(Expr(endo, f, A), Expr(endo, g, A)))
  }

  test("transform identity iso") {
    assert(sharing.transform($, _1).items == sharing.items)
    assert(bidi.transform($, _1).items == bidi.items)
    assert(firstclass.transform($, _1).items == firstclass.items)
  }

  test("execute laws") {
    import Instr.*

    assert(bidi.execute(Nil) == bidi)
    assert(ExprMap().execute(List(AppliedTo(1))) == ExprMap())
    assert(ExprMap(a -> 1).execute(List(AppliedTo(1)))
      .merge((l, r) => l)(ExprMap(b -> 1).execute(List(AppliedTo(1)))) ==
      ExprMap(a -> 1, b -> 1).execute(List(AppliedTo(1))))
    assert(bidi.execute(Seq(AppliedTo(1), AppliedTo(2)).iterator) ==
      bidi.execute(List(AppliedTo(1))).execute(List(AppliedTo(2))))
  }

  test("execute abstract") {
    import Instr.*

    assert(ExprMap(a -> 1, b -> 2).execute(List(AppliedTo(1))) ==
      ExprMap(Expr(f, a) -> 1, Expr(f, b) -> 2))

    assert(ExprMap(Expr(f, a, b) -> 1, Expr(Expr(g, f), Expr(g, A), Expr(g, B)) -> 2).execute(List(AppliedTo(3))) ==
      ExprMap(Expr(h, Expr(f, a, b)) -> 1, Expr(h, Expr(Expr(g, f), Expr(g, A), Expr(g, B))) -> 2))
  }

  test("execute prefix") {
    import Instr.*

    assert(ExprMap(a -> 1, b -> 2).execute(List(Prefix(1))) ==
      ExprMap(Expr(f, a) -> 1, Expr(f, b) -> 2))

    assert(ExprMap(Expr(a, b) -> 1, Expr(b, c) -> 2).execute(List(Prefix(1))) ==
      ExprMap(Expr(f, a, b) -> 1, Expr(f, b, c) -> 2))

    assert(ExprMap(Expr(a, b, c) -> 1, Expr(A, B, C) -> 2).execute(List(Prefix(1))) ==
      ExprMap(Expr(f, a, b, c) -> 1, Expr(f, A, B, C) -> 2))

    assert(ExprMap(Expr(g, a, b, c) -> 1, Expr(g, A, B, C) -> 2).execute(List(Prefix(1))) ==
      ExprMap(Expr(f, g, a, b, c) -> 1, Expr(f, g, A, B, C) -> 2))

    assert(ExprMap(Expr(h, g, a, b, c) -> 1, Expr(h, g, A, B, C) -> 2).execute(List(Prefix(1))) ==
      ExprMap(Expr(f, h, g, a, b, c) -> 1, Expr(f, h, g, A, B, C) -> 2))

    assert(ExprMap(f -> 1, g -> 2, h -> 3).execute(List(Prefix(12), Prefix(11), Prefix(10))) == inner)
  }
