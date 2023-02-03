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
    assert(ExprMap().execute(List(Apply(1))) == ExprMap())
    assert(ExprMap(a -> 1).execute(List(Apply(1)))
      .merge((l, r) => l)(ExprMap(b -> 1).execute(List(Apply(1)))) ==
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

  test("execute Elim") {
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

  sealed abstract class UnificationFailure(msg: String) extends RuntimeException(msg)

  case class ConflictingVars(pos: scala.collection.Set[Long]) extends UnificationFailure(s"Multiple vars: ${pos}")
  case class ConflictingVarAndApp(pos: Long, app: ExprMap[ExprMap[_]]) extends UnificationFailure(s"var: ${pos} and $app")


  test("mass unification") {
    // (A ◆ (f ◆) (f ⏴₂))
    // (A a ◆ (f (g ◆)))
    val e1 = App(App(App(A,a),$),App(f,App(g,$)))
    val e2 = App(App(App(A,$),App(f,$)),App(f,_2))
    val ex1 = ExprMap(e1 -> 1, e2 -> 2)
    val re = App(App(App(A,a),App(f,App(g,a))),App(f,App(g,a)))

    // ⦑⦑⦑⧼A: ⧼a: ⧼◆: ⦑⧼f: ⦑⧼g: ⧼◆⦒⦒⧽⦒⧽⦒,
    //       ◆: ⦑⧼f: ⧼◆: ⦑⧼f: ⧼⏴₂⦒⦒⧽⦒⦒⧽⦒⦒⧽⧽⧽

    // (((A a) (f (g a))) (f (g a)))

//    ExprMap(EM(ExprMap(EM(ExprMap(EM(ExprMap(EM(ExprMap(), LongMap(
//        A -> ExprMap(EM(ExprMap(),LongMap(
//          a -> ExprMap(EM(ExprMap(), LongMap(
//            $ -> ExprMap(EM(ExprMap(EM(ExprMap(), LongMap(
//              f -> ExprMap(EM(ExprMap(EM(ExprMap(), LongMap(
//                g -> ExprMap(EM(ExprMap(), LongMap(
//                  $ -> 1)))))), LongMap()))))), LongMap()))))),
//          $ -> ExprMap(EM(ExprMap(EM(ExprMap(), LongMap(
//            f -> ExprMap(EM(ExprMap(), LongMap(
//              $ -> ExprMap(EM(ExprMap(EM(ExprMap(), LongMap(
//                f -> ExprMap(EM(ExprMap(), LongMap(
//                  _2 -> 2)))))), LongMap())))))))), LongMap())))))))),LongMap())),LongMap())),LongMap()))


    def merge[V](em: EM[V], bindings: Seq[ExprMap[V]] = Nil): String =
      val pos = em.vars.view.filterKeys(_ > 0)
      val zero = em.vars.view.filterKeys(_ == 0)
      val neg = em.vars.view.filterKeys(_ < 0)
      if pos.sizeIs > 1 then throw ConflictingVars(pos.keySet)
      if pos.nonEmpty && em.apps.nonEmpty then throw ConflictingVarAndApp(pos.keys.head, em.apps.asInstanceOf)
      if neg.isEmpty then
        if pos.isEmpty then
          if zero.isEmpty then
            if em.apps.isEmpty then "empty"
            else s"app ${em.apps.prettyStructuredSet(false)}"
          else
            if em.apps.isEmpty then "registering any"
            else s"setting ${bindings.length} to ${em.apps.prettyStructuredSet(false)}"
        else
          if zero.isEmpty then s"skipping over constant ${pos.keys.head}"
          else s"setting ${bindings.length} to ${pos.keys.head}"
      else
        if pos.isEmpty then
          if zero.isEmpty then
            if em.apps.isEmpty then s"unify bindings ${neg.keySet}"
            else s"unify bindings ${neg.keySet} and app ${em.apps.prettyStructuredSet(false)}"
          else
            if em.apps.isEmpty then s"equate ${bindings.length} to ${neg.keySet}"
            else s"equate ${bindings.length} to ${neg.keySet} and ${em.apps.prettyStructuredSet(false)}"
        else
          if zero.isEmpty then
            s"setting bindings ${neg.keySet} to ${pos.keys.head}"
          else
            s"setting ${bindings.length} and ${neg.keySet} to ${pos.keys.head}"


    //    LongMap(
    //      A -> ExprMap(EM(ExprMap(),
    //        LongMap(
    //          $ -> ExprMap(EM(ExprMap(EM(ExprMap(null),LongMap(1 -> ExprMap(EM(ExprMap(null),LongMap(0 -> ExprMap(EM(ExprMap(EM(ExprMap(null),LongMap(1 -> ExprMap(EM(ExprMap(null),LongMap(-2 -> 2)))))),LongMap())))))))),LongMap())),
    //          a -> ExprMap(EM(ExprMap(null),LongMap(0 -> ExprMap(EM(ExprMap(EM(ExprMap(null),LongMap(1 -> ExprMap(EM(ExprMap(EM(ExprMap(null),LongMap(2 -> ExprMap(EM(ExprMap(null),LongMap(0 -> 1)))))),LongMap()))))),LongMap())))))))))))
    assert(merge(ex1.em.apps.em.apps.em.apps.em.asInstanceOf) == eids"skipping over constant $A")

    //    LongMap(
//      $ -> ExprMap(EM(ExprMap(EM(ExprMap(),LongMap(f -> ExprMap(EM(ExprMap(),LongMap($ -> ExprMap(EM(ExprMap(EM(ExprMap(),LongMap(f -> ExprMap(EM(ExprMap(),LongMap(_2 -> 2)))))),LongMap())))))))),LongMap())),
//      a -> ExprMap(EM(ExprMap(),LongMap($ -> ExprMap(EM(ExprMap(EM(ExprMap(),LongMap(f -> ExprMap(EM(ExprMap(EM(ExprMap(),LongMap(g -> ExprMap(EM(ExprMap(),LongMap($ -> 1)))))),LongMap()))))),LongMap()))))))
    assert(merge(ex1.em.apps.em.apps.em.apps.em.vars(A.leftMost).em.asInstanceOf) == eids"setting 0 to $a")


//    ExprMap(EM(
//      ExprMap(EM(ExprMap(),LongMap(f -> ExprMap(EM(ExprMap(),LongMap($ -> ExprMap(EM(ExprMap(EM(ExprMap(null),LongMap(1 -> ExprMap(EM(ExprMap(null),LongMap(-2 -> 2)))))),LongMap())))))))),
//      LongMap($ -> ExprMap(EM(ExprMap(EM(ExprMap(null),LongMap(1 -> ExprMap(EM(ExprMap(EM(ExprMap(null),LongMap(2 -> ExprMap(EM(ExprMap(null),LongMap(0 -> 1)))))),LongMap()))))),LongMap())))))
    // ⧼◆: ⦑⧼1: ⦑⧼2: ⧼◆⦒⦒⧽⦒⧽
    // |⧼1: ⧼◆: ⦑⧼1: ⧼⏴₂⦒⦒⧽⦒⦒⧽
    assert(merge(
      ex1.em.apps.em.apps.em.apps.em.vars(A.leftMost).em.vars($.leftMost).merge((l, r) => l)(
      ex1.em.apps.em.apps.em.apps.em.vars(A.leftMost).em.vars(a.leftMost)).em) == "setting 0 to ⧼1: ⧼◆: ⦑⧼1: ⧼⏴₂⦒⦒⧽⦒⦒")
  }