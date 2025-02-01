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

//test("many updates size") {
//  val em = ExprMap[Int](EM(ExprMap(), collection.mutable.LongMap.fromZip(0L until 1_000_000L, 0 until 1_000_000))).execute(Seq(Instr.AppliedTo(1)))
//  val number = Expr.Var(1)
//
//  assert(em.get(Expr.App(number, Expr.Var(666_666))).contains(666_666))
//  assert(em.size == 1_000_000)
//}


  test("many updates size") {
    val em = ExprMap[Int]()
    val number = Expr.Var(1)

    for i <- 0 until 1_000_000 do
      em.update(Expr.App(number, Expr.Var(i)), i)

    assert(em.get(Expr.App(number, Expr.Var(666_666))).contains(666_666))
    assert(em.size == 1_000_000)
  }

//  test("ExprMap deep updates size") {
//    val em = ExprMap[Int](EM(ExprMap(), collection.mutable.LongMap.fromZip(0L until 1_000, 0 until 1_000))).execute(Seq.fill(1000)(Instr.AppliedTo(1)))
//    val F = Expr.Var(1)
//
//    def wrap(e: Expr, depth: Int): Expr =
//      if depth == 0 then e else wrap(Expr.App(F, e), depth - 1)
//
//    //    for i <- 0 until 1_000 do
//    //      em.update(wrap(Expr.Var(i), 1000), i)
//
//    assert(em.get(wrap(Expr.Var(666), 1000)).contains(666))
//    assert(em.size == 1000)
//  }

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
    // TODO, new order
//    assert(sharing.json == """{"@":{"@":{"100":{"10":{"11":1},"@":{"1":{"0":{"@":{"10":{"-1":2}}},"11":{"12":3}},"@":{"2":{"0":{"0":{"@":{"@":{"10":{"@":{"12":{"-1":{"-2":4}}}}}}}}}}}}}}}""")
//    assert(inner.json == """{"@":{"@":{"@":{"10":{"11":{"12":{"1":1,"2":2,"3":3}}}}}}}""")
  }

  test("ExprMap prettyStructured") {
    assert(ExprMap(a -> 1, b -> 2).prettyStructured(colored=false) == eids"⦑$a: 1, $b: 2⧽")
    assert(ExprMap(a -> 1, Expr(f, b) -> 2).prettyStructured(colored=false) == eids"⧼⦑$f: ⦑$b: 2⧽⧽|$a: 1⧽")

    assert(sharing.prettyStructured(colored=false) == eids"⧼⧼⦑${`=`}: ⧼⧼⦑$g: ⦑◆: ⦑◆: ⧼⧼⦑$a: ⧼⦑$c: ⦑⏴₁: ⦑⏴₂: 4⧽⧽⧽⦒⧽⦒⦒⧽⧽⧽|$f: ⦑◆: ⧼⦑$a: ⦑⏴₁: 2⧽⧽⦒, $b: ⦑$c: 3⧽⧽⧽|$a: ⦑$b: 1⧽⧽⧽⦒⦒")
    assert(inner.prettyStructured(colored=false) == eids"⧼⧼⧼⦑$a: ⦑$b: ⦑$c: ⦑$f: 1, $g: 2, $h: 3⧽⧽⧽⧽⦒⦒⦒")
  }

  test("ExprMap prettyStructuredSet") {
    assert(ExprMap(a -> 1, b -> 2).prettyStructuredSet(colored = false) == eids"⦑$a, $b⧽")
    assert(ExprMap(a -> 1, Expr(f, b) -> 2).prettyStructuredSet(colored = false) == eids"⧼⦑$f: ⦑$b⧽⧽|$a⧽")

    assert(sharing.prettyStructuredSet(colored = false) == eids"⧼⧼⦑${`=`}: ⧼⧼⦑$g: ⦑◆: ⦑◆: ⧼⧼⦑$a: ⧼⦑$c: ⦑⏴₁: ⦑⏴₂⧽⧽⧽⦒⧽⦒⦒⧽⧽⧽|$f: ⦑◆: ⧼⦑$a: ⦑⏴₁⧽⧽⦒, $b: ⦑$c⧽⧽⧽|$a: ⦑$b⧽⧽⧽⦒⦒")
    assert(inner.prettyStructuredSet(colored = false) == eids"⧼⧼⧼⦑$a: ⦑$b: ⦑$c: ⦑$f, $g, $h⧽⧽⧽⧽⦒⦒⦒")
  }

  test("ExprMap prettyStructuredSet tree") {
    println(ExprMap(a -> 1, b -> 2).prettyStructuredSet(colored = false, tree = true))
    println(ExprMap(a -> 1, Expr(f, b) -> 2).prettyStructuredSet(colored = false, tree = true))

    println(sharing.prettyStructuredSet(colored = false, tree = true))
    println(inner.prettyStructuredSet(colored = false, tree = true))
  }

  test("ExprMap prettyListing") {
    assert(ExprMap(a -> 1, b -> 2).prettyListing(colored = false) == eids"$a\n$b")
    println(ExprMap(a -> 1, Expr(f, b) -> 2).prettyStructuredSet(colored = false))
    assert(ExprMap(a -> 1, Expr(f, b) -> 2).prettyStructuredSet(colored = false) == eids"⧼⦑$f: ⦑$b⧽⧽|$a⧽")

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
    assert(ExprMap().execute(List(Apply(1))) == ExprMap())
    assert(ExprMap(a -> 1).execute(List(Apply(1)))
      .union(ExprMap(b -> 1).execute(List(Apply(1)))) ==
      ExprMap(a -> 1, b -> 1).execute(List(Apply(1))))
    assert(bidi.execute(List(Apply(1), Apply(2))) ==
      bidi.execute(List(Apply(1))).execute(List(Apply(2))))
  }

  test("execute Apply") {
    import Instr.*

    assert(ExprMap(a -> 1, b -> 2).execute(List(Apply(1))) ==
      ExprMap(Expr(f, a) -> 1, Expr(f, b) -> 2))

    assert(ExprMap(Expr(f, a, b) -> 1, Expr(Expr(g, f), Expr(g, A), Expr(g, B)) -> 2).execute(List(Apply(3))) ==
      ExprMap(Expr(h, Expr(f, a, b)) -> 1, Expr(h, Expr(Expr(g, f), Expr(g, A), Expr(g, B))) -> 2))
  }

  test("execute Prepend") {
    import Instr.*

    assert(ExprMap(a -> 1, b -> 2).execute(List(Prepend(1))) ==
      ExprMap(Expr(f, a) -> 1, Expr(f, b) -> 2))

    assert(ExprMap(Expr(a, b) -> 1, Expr(b, c) -> 2).execute(List(Prepend(1))) ==
      ExprMap(Expr(f, a, b) -> 1, Expr(f, b, c) -> 2))

    assert(ExprMap(Expr(a, b, c) -> 1, Expr(A, B, C) -> 2).execute(List(Prepend(1))) ==
      ExprMap(Expr(f, a, b, c) -> 1, Expr(f, A, B, C) -> 2))

    assert(ExprMap(Expr(g, a, b, c) -> 1, Expr(g, A, B, C) -> 2).execute(List(Prepend(1))) ==
      ExprMap(Expr(f, g, a, b, c) -> 1, Expr(f, g, A, B, C) -> 2))

    assert(ExprMap(Expr(h, g, a, b, c) -> 1, Expr(h, g, A, B, C) -> 2).execute(List(Prepend(1))) ==
      ExprMap(Expr(f, h, g, a, b, c) -> 1, Expr(f, h, g, A, B, C) -> 2))

    assert(ExprMap(f -> 1, g -> 2, h -> 3).execute(List(Prepend(12), Prepend(11), Prepend(10))) == inner)
  }

  test("execute Tail") {
    import Instr.*

    val plain = ExprMap(a -> 2, b -> 3, c -> 5)
    val wrapped = ExprMap(Expr(f, a) -> 2, Expr(f, b) -> 3, Expr(f, c) -> 5)
    val wrappedA = ExprMap(Expr(A, f, a) -> 2, Expr(A, g, b) -> 3, Expr(A, h, c) -> 5)
    val wrappedAB = ExprMap(Expr(B, A, f, a) -> 2, Expr(B, A, g, b) -> 3, Expr(B, A, h, c) -> 5)
    val wrappedABC = ExprMap(Expr(C, B, A, f, a) -> 2, Expr(C, B, A, g, b) -> 3, Expr(C, B, A, h, c) -> 5)

    assert(plain.execute(List(Tail(1))) == ExprMap())
    assert(wrapped.execute(List(Tail(1))) == plain)
    assert(wrappedA.execute(List(Tail(20))) == ExprMap(Expr(f, a) -> 2, Expr(g, b) -> 3, Expr(h, c) -> 5))
    assert(wrappedAB.execute(List(Tail(21))) == wrappedA)
    assert(wrappedABC.execute(List(Tail(22))) == wrappedAB)

    assert(ExprMap(Expr(f, a) -> 1, Expr(g, b) -> 2).execute(List(Tail(1))) ==
      ExprMap(a -> 1))
    assert(ExprMap(Expr(f, a) -> 1, Expr(f, a, b) -> 2).execute(List(Tail(1))) ==
      ExprMap(a -> 1, Expr(a, b) -> 2))
    assert(ExprMap(Expr(f, a, b) -> 1, Expr(f, a, b, c) -> 2).execute(List(Tail(1))) ==
      ExprMap(Expr(a, b) -> 1, Expr(a, b, c) -> 2))
    assert(ExprMap(Expr(f, a, b, A) -> 1, Expr(f, a, b, c, A) -> 2).execute(List(Tail(1))) ==
      ExprMap(Expr(a, b, A) -> 1, Expr(a, b, c, A) -> 2))
  }

  test("execute Unapply") {
    import Instr.*

    assert(ExprMap(Expr(A, B) -> 1).execute(List(Unapply(20))) ==
      ExprMap(B -> 1))
    assert(ExprMap(Expr(A, Expr(A, B)) -> 1).execute(List(Unapply(20))) ==
      ExprMap(Expr(A, B) -> 1))
    assert(ExprMap(Expr(A, Expr(A, Expr(A, B))) -> 1).execute(List(Unapply(20))) ==
      ExprMap(Expr(A, Expr(A, B)) -> 1))

//    println(ExprMap(Expr(A, Expr(A, Expr(A, B)), C) -> 1).execute(List(Unapply(20))).prettyStructuredSet())
//    println(ExprMap(Expr(A, Expr(A, B)) -> 1).prettyStructuredSet())
  }

  test("execute Drop") {
    import Instr.*

    println(ExprMap(
      Expr(A, B) -> 1,
      Expr(B, C) -> 2,
      Expr(Expr(A, B), A) -> 3,
      Expr(Expr(C, B, A), a) -> 4,
      //      Expr(A, B, C, A) -> 3
    ).execute(List(Drop)).prettyListing())

    assert(ExprMap(Expr(A, B) -> 1).execute(List(Drop)) ==
      ExprMap(B -> 1))
    assert(ExprMap(Expr(A, Expr(A, B)) -> 1).execute(List(Drop)) ==
      ExprMap(Expr(A, B) -> 1))
    assert(ExprMap(Expr(A, Expr(A, Expr(A, B))) -> 1).execute(List(Drop)) ==
      ExprMap(Expr(A, Expr(A, B)) -> 1))
//    assert()
  }

  test("execute DropHead") {
    import Instr.*

//    println(ExprMap(
//      Expr(A, C, C, B) -> 1,
//      Expr(B, C, C, B) -> 2,
////      Expr(A, B, C, A) -> 3
//    ).execute(List(Tail(20), DropHead, DropHead)).prettyListing())

    assert(ExprMap(Expr(A, B) -> 1).execute(List(DropHead)) ==
      ExprMap(B -> 1))
    assert(ExprMap(Expr(A, A, B) -> 1).execute(List(DropHead)) ==
      ExprMap(Expr(A, B) -> 1))
    assert(ExprMap(Expr(A, A, A, B) -> 1).execute(List(DropHead)) ==
      ExprMap(Expr(A, A, B) -> 1))

//    assert(ExprMap(Expr(A, C, C, B) -> 1).execute(List(DropHead, DropHead, DropHead)))
  }

  test("explicit bindings") {
    val Sam = Var(1000)
    val Sigfried = Var(1001)

    val posses = Var(1010)
    val likes = Var(1011)

    val blue = Var(1020)
    val red = Var(1021)

    val stuff = Var(1030)
    val balloon = Var(1031)
    val car = Var(1032)

    val bindings = Var(1100)

    val em = ExprMap(
      Expr(posses, Sam, balloon) -> 1,
      Expr(posses, Sigfried, car) -> 2,

      Expr(likes, Sam, Expr(blue, stuff)) -> 10,
      Expr(likes, Sigfried, Expr(red, stuff)) -> 11,
    )

    // normal querying
    assert(em.transform(Expr($, Sam, $), Expr(_1, _2)).keys.toSet == Set(Expr(posses, balloon), Expr(likes, Expr(blue, stuff))))

    // get bindings
    val bds = em.transform(Expr($, Sam, $), Expr(bindings, Expr($, Sam, $), _1, _2))
    // stored as
    // ⧼⧼⧼⦑bindings: ⧼⧼⦑$: ⦑Sam: ⦑$:
    //    ⦑posses: ⦑balloon⦒,
    //    likes: ⧼⦑blue: ⦑stuff⦒⦒⧽⦒⦒⦒⦒⧽⧽⦒⧽⧽⧽
    assert(bds.keys.toSet == Set(
      Expr(bindings, Expr($, Sam, $), posses, balloon),
      Expr(bindings, Expr($, Sam, $), likes, Expr(blue, stuff))))
    // reduce bindings
    val res = for case Expr(`bindings`, template, templateBds: _*) <- bds.keys.toSet
      yield template.substRel(templateBds)

    assert(res == Set(Expr(posses, Sam, balloon), Expr(likes, Sam, Expr(blue, stuff))))
  }

  test("applied appliedWith") {
    val fem = ExprMap(f -> 1, Expr(h, g) -> 2)
    val aem = ExprMap(A -> 10, Expr(B, A) -> 20)

    val rem = ExprMap(
      Expr(f, A) -> 11,
      Expr(f, Expr(B, A)) -> 21,
      Expr(Expr(h, g), A) -> 12,
      Expr(Expr(h, g), Expr(B, A)) -> 22,
    )

    assert(fem.applied(aem).keys.toSet == rem.keys.toSet)
    assert(fem.appliedWith(_ + _)(aem) == rem)

    val fem2 = ExprMap(Expr.nest(f, g, h) -> 1)
    val aem2 = ExprMap(A -> 10, B -> 20)
    val rem2 = ExprMap(
      Expr(Expr.nest(f, g, h), A) -> 11,
      Expr(Expr.nest(f, g, h), B) -> 21,
    )

    assert(fem2.applied(aem2).keys.toSet == rem2.keys.toSet)
    assert(fem2.appliedWith(_ + _)(aem2) == rem2)
  }

  test("appliedWithBench") {
    ()
  }

  test("union unionWith") {
    val l1 = ExprMap(a -> 1, b -> 2, A -> 3, B -> 4)
    val r1 = ExprMap(f -> 10, g -> 20, A -> 30, B -> 40)
    val em1 = ExprMap(a -> 1, b -> 2, A -> 33, B -> 44, f -> 10, g -> 20)

    assert(l1.union(r1).keys == em1.keys)
    assert(l1.unionWith(_ + _)(r1) == em1)

    val l2 = ExprMap(e1 -> 1, e2 -> 2)
    val r2 = ExprMap(e2 -> 20, e3 -> 30)
    val em2 = ExprMap(e1 -> 1, e2 -> 22, e3 -> 30)

    assert(l2.union(r2).keys == em2.keys)
    assert(l2.unionWith(_ + _)(r2) == em2)
  }

  test("intersection intersectionWith") {
    val l1 = ExprMap(a -> 1, b -> 2, A -> 3, B -> 4)
    val r1 = ExprMap(f -> 10, g -> 20, A -> 30, B -> 40)
    val em1 = ExprMap(A -> 33, B -> 44)

    assert(l1.intersection(r1).keys == em1.keys)
    assert(l1.intersectionWith(_ + _)(r1) == em1)

    val l2 = ExprMap(e1 -> 1, e2 -> 2)
    val r2 = ExprMap(e2 -> 20, e3 -> 30)
    val em2 = ExprMap(e2 -> 22)

    assert(l2.intersection(r2).keys == em2.keys)
    assert(l2.intersectionWith(_ + _)(r2) == em2)
  }

  test("subtract") {
    val l1 = ExprMap(a -> 1, b -> 2, A -> 3, B -> 4)
    val r1 = ExprMap(f -> 10, g -> 20, A -> 30, B -> 40)
    val em1 = ExprMap(a -> 33, b -> 44)

    assert(l1.subtract(r1).keys == em1.keys)

    val l2 = ExprMap(Expr(f, a) -> 1, b -> 2, Expr(g, A) -> 3, Expr(g, B) -> 4)
    val r2 = ExprMap(f -> 10, Expr(g, A) -> 20, Expr(h, A) -> 30, B -> 40)
    val em2 = ExprMap(b -> 22, Expr(f, a) -> 33, Expr(g, B) -> 44)

    println("SSUB")
    println(l2.subtract(r2).prettyListing())
    assert(l2.subtract(r2).keys == em2.keys)

    // TODO make more exhaustive
  }

  test("getAt") {
//     sharing.getAt(List(Some(`=`.leftMost)))  // no free ='s
//     sharing.getAt(List(None, Some(`=`.leftMost)))  // no 1-argument ='s
    assert(sharing.getAt(List(None, None, Some(`=`.leftMost))) == Left(sharing.em.apps.em.apps.em.vars(`=`.leftMost).em)) // here's the content
//    sharing.getAt(List(None, None, None, Some(`=`.leftMost)))  // no 3-argument versions
    assert(sharing.getAt(List(None, None, Some(`=`.leftMost), Some(a.leftMost))) == Left(EM.single(b, 1))) // here's ⦑b: 1⧽
//    sharing.getAt(List(None, None, Some(`=`.leftMost), Some(a.leftMost), None)) // no trees originating at a
    assert(sharing.getAt(List(None, None, Some(`=`.leftMost), Some(a.leftMost), Some(b.leftMost))) == Right(1))

    assert(sharing.getAt(List(None, None, Some(`=`.leftMost), None, Some(f.leftMost))) == Left(EM(ExprMap(), VarMap(
      $.leftMost.toLong -> ExprMap(Expr(a, _1) -> 2),
      b.leftMost.toLong -> ExprMap(c -> 3),
    ))))
  }

  test("setAt") {
    val blank = ExprMap[Int]()

    blank.setAt(List(None, Some(f.leftMost), Some(b.leftMost)), ???, 42)
    assert(blank.items.toSet == Set(Expr(f, b) -> 42))
    blank.setAt(List(None, Some(f.leftMost), None, Some(g.leftMost), Some(b.leftMost)), ???, 43)
    assert(blank.items.toSet == Set(Expr(f, b) -> 42, Expr(f, Expr(g, b)) -> 43))
    blank.setAt(List(None, Some(f.leftMost), Some(c.leftMost)), ???, 44)
    assert(blank.items.toSet == Set(Expr(f, b) -> 42, Expr(f, c) -> 44, Expr(f, Expr(g, b)) -> 43))
    blank.setAt(List(None, Some(f.leftMost), None, Some(h.leftMost)), EM(ExprMap(), VarMap(a.leftMost.toLong -> 45,  c.leftMost.toLong -> 46)), ???)
    assert(blank.items.toSet == (Set(Expr(f, b) -> 42, Expr(f, c) -> 44, Expr(f, Expr(g, b)) -> 43) union
                                 Set(Expr(f, Expr(h, a)) -> 45, Expr(f, Expr(h, c)) -> 46)))

    val partialf_ = partialf.copy()
    partialf_.setAt(List(None, None, Some(`=`.leftMost), None, Some(`f`.leftMost)),
      EM(ExprMap(), VarMap(A.leftMost.toLong -> ExprMap(EM(ExprMap(), VarMap(a.leftMost.toLong -> 20))),
                           B.leftMost.toLong -> ExprMap(EM(ExprMap(), VarMap(b.leftMost.toLong -> 21))),
                           C.leftMost.toLong -> ExprMap(EM(ExprMap(), VarMap(c.leftMost.toLong -> 22))))), ???)
    assert(partialf_.items.toSet == partialf.items.toSet
      .excl(Expr(`=`, Expr(f, B), b) -> 20)
      .union(Set(
        Expr(`=`, Expr(f, A), a) -> 20,
        Expr(`=`, Expr(f, B), b) -> 21,
        Expr(`=`, Expr(f, C), c) -> 22)))
  }

  test("instruction unification") {
    def experiment(q: Expr) =
      println(s"--- matching ${q.pretty()} ---")
      val res = variety.indiscriminateBidirectionalMatching(q)
      println(res.prettyStructuredSet())
      println(res.prettyListing())
      val instrs = q.indiscriminateBidirectionalMatchingTrace()
      println("--- instructions ---")
      instrs.foreach(println)
      println("--- executing --")
      val res_ = variety.execute(instrs, debug=1)
      println("--- result --")
      println(res_.prettyStructuredSet())
      println(res_.prettyListing())
      println()
      println()
      assert(res == res_)

    experiment(Expr(Var(1000), Var(1001), $, Var(1003)))
    experiment(Expr.nest(C, B, B, $))
    experiment(Expr(`=`, $, $))
    experiment(Expr($, A, Expr(B, A)))
  }

  test("addresses") {
    println("addresses")
    inner.addresses().foreach(println)
  }
end ExprMapTest
