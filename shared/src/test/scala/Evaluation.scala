package be.adamv.cz2

import munit.FunSuite

class EvaluationTest extends FunSuite:
  import ExprExamples.*
  import ExprMapExamples.*

  test("linear") {
    import EvaluationAlgorithms.*
    {
      given ExprMap[Int] = simplelinear
      /*
      Expr(`=`, A, B)                      // val A = B
      Expr(`=`, Expr(f, $x), Expr(a, $x))  // def f(x) = a(x)

      - Unify
          Expr(`=`, Expr(f, $x), Expr(a, $x))
          Expr(`=`, Expr(f, c), $y)
      -  Map($y = Expr(a, c))($y) == Expr(a, c)

      Expr(`=`, Expr(g, Expr(b, $x)), $x)  // def g = { case b(x) => x }
      */

      assert(eval(A) == B)
      assert(eval(Expr(f, c)) == Expr(a, c))
      assert(eval(Expr(g, Expr(b, c))) == c)
      assert(eval(Expr(g, Expr(b, Expr(A, Expr(f, c), c)))) == App(App(B, App(a, c)), c))
    }
  }

  test("multivalued") {
    import EvaluationAlgorithms.*
    {
      given ExprMap[Int] = bidi
      assert(lookupMulti(Expr(f, b)) == Set(Expr(a, b)))
      assert(lookupMulti(Expr(Var(42), b)).isEmpty)
      assert(lookupMulti(Expr(h, Expr(`,`, Expr(a, Expr(a, b)), Expr(a, Expr(a, b))))) ==
             Set(Expr(a, Expr(h, Expr(`,`, Expr(a, b), Expr(a, Expr(a, b)))))))
      assert(evalMulti(Expr(h, Expr(`,`, Expr(a, Expr(a, b)), Expr(a, Expr(a, b))))) ==
             Set(Expr(a, Expr(a, Expr(a, Expr(a, b))))))
    }
    {
      given ExprMap[Int] = prob
      assert(evalMulti(Expr(g, A)) == Set(Var(1000), Var(1001)))
      assert(evalMulti(Expr(g, B)) == Set(Var(1010), Var(1011), Var(1012)))
      assert(evalMulti(Expr(g, C)) == Set(Var(1000), Var(1001), Var(1010), Var(1011), Var(1012)))
    }
    {
      given ExprMap[Int] = partialf
      assert(evalMulti(Expr(f, A)) == Set(Expr(f, C), b))
    }
  }

  test("traced".ignore) {
    import ValueEvaluationAlgorithms.pathHash.*

    {
      given ExprMap[Long] = bidi.map(hash)

      assert(apply(Expr(h, Expr(`,`, Expr(a, Expr(a, b)), Expr(a, Expr(a, b))))).items.toSet == Set((Expr.nest(a, a, a, a, b), 0x742d754a1b9ce7edL)))
    }
    {
      given ExprMap[Long] = prob.map(hash)

      assert(apply(Expr(g, A)).items.toSet == Set((Var(1001),0xc619116b09aad548L), (Var(1000),0xe03b5cf86ece28bfL)))
      assert(apply(Expr(g, B)).items.toSet == Set((Var(1010),0x7cbdeb27e2d7b613L), (Var(1011),0x9b3afa36fa320827L), (Var(1012),0x63226a9bd0733a6aL)))
      assert(apply(Expr(g, C)).items.toSet == Set((Var(1011),0x724e8da2afe9fb77L), (Var(1010),0xb6b26988ff8f85bbL), (Var(1000),0x1d2ef9f5a8a5dd17L), (Var(1012),0x37ec1bcf1455895eL), (Var(1001),0x2a7d215e9d65e500L)))
    }
    {
      given ExprMap[Long] = firstclass.map(hash)
      assert(apply(Expr(f, a)).items.toSet == Set((a, 0x849516bdae7745a2L)))
      assert(apply(Expr(f, b)).items.toSet == Set((c, 0x29fa1f482da03bedL)))
      assert(apply(Expr(f, c)).items.toSet == Set((a, 0xac38e8d604c92964L)))
    }
    {
      given ExprMap[Long] = simplelinear.map(hash)

      assert(unapply(Expr(f, c)).contains(Expr(a, c) -> 0x4f36a2257a7fa018L))
      assert(unapply(Expr(g, Expr(b, c))).contains(c -> 0x7775bd495aa37d53L))
      assert(unapply(Expr(g, Expr(b, Expr(A, Expr(f, c), c)))).contains(Expr(B, Expr(a, c), c) -> 0x611c4c9a9e2e562L))
    }
  }

  test("grounded".ignore) {
    import ValueEvaluationAlgorithms.pathHash

    {
      val groundedAB = Var(30)
      val groundedXA = Var(31)
      val groundedBundle = Var(40)
      val pfs = collection.mutable.Map.empty[Int, ExprMap[Long] => ExprMap[Long]]
      var pc = 40
      given PartialFunction[Int, ExprMap[Long] => ExprMap[Long]] = {
        case 30 => em => ExprMap.from(em.items.map{
          case (`A`, av) => (B, ~av)
          case (a, av) => (Expr(Var(30), a), av)
        })
        case 31 => em => ExprMap(A -> em.values.reduce(_ | _))
        case 40 => em => ExprMap.from(em.items.map((e1, s1) =>
          pc += 1
          pfs(pc) = em2 => ExprMap.from(em2.items.map((e2, s2) => Expr(`,`, e1, e2) -> (s1 + s2)))
          Var(pc) -> 3*s1
        ))
        case pfs(handler) => handler
      }
      given ExprMap[Long] = ExprMap(
        Expr(`=`, Expr(groundedAB, A), C) -> 1,
        Expr(`=`, Expr(f, A), a) -> 10,
        Expr(`=`, h, a) -> 20,
        Expr(`=`, h, b) -> 21
      ).map(hash)

      assert(pathHash.evalGrounded(Expr(groundedAB, A), 0xea623317b5e84485L).items.toSet ==
        Set(B -> 0x159dcce84a17bb7aL))
      assert(pathHash.evalGrounded(Expr(f, A), 0xea623317b5e84485L).items.toSet ==
        Set(a -> 0xf971744492872e27L))
      assert(pathHash.evalGrounded(Expr(groundedAB, B), 0xea623317b5e84485L).items.toSet ==
        Set(Expr(groundedAB, B) -> 0xea623317b5e84485L))
      assert(pathHash.evalGrounded(Expr(groundedXA, h), 0xea623317b5e84485L).items.toSet ==
        Set(A -> 0x7767f7ddbeffcfcfL))
      assert(pathHash.evalGrounded(Expr(groundedBundle, a, b), 0x7775bd495aa37d53L).items.toSet ==
        Set(Expr(`,`, a, b) -> 0x21f53973349aba6eL))
    }
    {
      val transform = Var(30)
      val rel = Var(40)
      val relOp = Var(41)
      val Op = Var(50)
      val pfs = collection.mutable.Map.empty[Int, ExprMap[Long] => ExprMap[Long]]
      var pc = 10000
      given PartialFunction[Int, ExprMap[Long] => ExprMap[Long]] = {
        case 30 => em => ExprMap.from(em.items.map((e1, s1) =>
          pc += 1
          pfs(pc) = em2 => ExprMap.from(em2.items.flatMap((e2, s2) =>
            space.transform(e1, e2).items
          ))
          Var(pc) -> 3*s1
        ))
        case pfs(handler) => handler
      }
      given space: ExprMap[Long] = ExprMap(
        Expr(rel, A, B) -> 1,
        Expr(rel, a, b) -> 2,
        Expr(relOp, C, B) -> 10,
        Expr(`=`, Expr(Op, rel), relOp) -> 20,
        Expr(`=`, Expr(Op, relOp), rel) -> 21,
        Expr(`=`, Expr(f, $), Expr(transform, Expr(_1, $, $), Expr(Expr(Op, _1), _3, _2))) -> 30
      ).map(hash)

      assert(pathHash.evalGrounded(Expr(f, rel), 0xc1d4f1553eecf0fL).keys.toSet == Set(Expr(relOp, B, A), Expr(relOp, b, a)))
      assert(pathHash.evalGrounded(Expr(f, relOp), 0xc1d4f1553eecf0fL).keys.toSet == Set(Expr(rel, B, C)))
    }
  }

  test("grounded data") {
    import ValueEvaluationAlgorithms.pathHash

    {
      // content addressed map

      val camap = collection.mutable.Map.empty[Int, String]

      def str(s: String): Expr =
        val h = s.hashCode
        camap(h) = s
        Var(h)

      val greeting = Var(1000)
      val concat = Var(1010)

      var pc = 10000
      val pfs = collection.mutable.Map.empty[Int, ExprMap[Long] => ExprMap[Long]]
      given PartialFunction[Int, ExprMap[Long] => ExprMap[Long]] = {
        case 1010 => em =>
          ExprMap.from(em.items.map((e1, s1) =>
            pc += 1
            pfs(pc) = em2 => ExprMap.from(em2.items.flatMap((e2, s2) => (e1, e2) match
              case (Var(camap(str1)), Var(camap(str2))) => Some(str(str1 + str2) -> (s1*31 + s2))
              case _ => None
            ))
            Var(pc) -> 3 * s1
          ))
        case pfs(handler) => handler
      }

      given space: ExprMap[Long] = ExprMap(
        Expr(`=`, greeting, str("hi")) -> 1,
        Expr(`=`, greeting, str("hello")) -> 2,
        Expr(`=`, f, Expr(concat, str("<"), str(">"))) -> 10,
        Expr(`=`, Expr(g, $), Expr(concat, greeting, Expr(concat, str(" "), _1))) -> 20
      ).map(hash)


      def unsafeValue(e: Expr): String = e match
        case Var(camap(s)) => s
      assert(pathHash.evalGrounded(f, 0xc1d4f1553eecf0fL).keys.map(unsafeValue).toSet == Set("<>"))
      assert(pathHash.evalGrounded(greeting, 0xc1d4f1553eecf0fL).keys.map(unsafeValue).toSet == Set("hi", "hello"))
      assert(pathHash.evalGrounded(Expr(g, str("world")), 0xc1d4f1553eecf0fL).keys.map(unsafeValue).toSet == Set("hi world", "hello world"))
    }
    {
      // counted in EM

      enum Grounded:
        case Value(s: String)
        case Expression
        case Conflict

      val greeting = Var(1000)
      val concat = Var(1010)
      val groundedValue = Var(1020)

      var pc = 10000
      val pfs = collection.mutable.Map.empty[Int, ExprMap[Grounded] => ExprMap[Grounded]]

      given space: ExprMap[Grounded] = ExprMap()

      var c = 20000

      def str(s: String): Expr =
        c += 1
        space.update(Expr(groundedValue, Var(c)), Grounded.Value(s))
        Var(c)

      space.update(Expr(`=`, greeting, str("hi")), Grounded.Expression)
      space.update(Expr(`=`, greeting, str("hello")), Grounded.Expression)
      space.update(Expr(`=`, f, Expr(concat, str("<"), str(">"))), Grounded.Expression)
      space.update(Expr(`=`, Expr(g, $), Expr(concat, greeting, Expr(concat, str(" "), _1))), Grounded.Expression)

      def unsafeValue(e: Expr): String = space.getUnsafe(Expr(groundedValue, e)) match
        case Grounded.Value(s) => s

      given PartialFunction[Int, ExprMap[Grounded] => ExprMap[Grounded]] = {
        case 1010 => em =>
          ExprMap.from(em.items.map((e1, s1) =>
            pc += 1
            pfs(pc) = em2 => ExprMap.from(em2.items.flatMap((e2, s2) =>
              val str1 = unsafeValue(e1)
              val str2 = unsafeValue(e2)
              Some(str(str1 + str2) -> Grounded.Expression)
            ))
            Var(pc) -> Grounded.Expression
          ))
        case pfs(handler) => handler
      }

      val vea = new ValueEvaluationAlgorithms[Grounded] {
        def handleLookup(emv: Grounded, ev: Grounded): Grounded = emv
        def handleMerge(fv: Grounded, av: Grounded): Grounded = if fv == av then fv else Grounded.Conflict
        override def apply(e: Expr)(using s: ExprMap[Grounded]) = ???
        override def unapply(e: Expr)(using s: ExprMap[Grounded]) = ???
      }

      assert(vea.evalGrounded(f, Grounded.Expression).keys.map(unsafeValue).toSet == Set("<>"))
      assert(vea.evalGrounded(greeting, Grounded.Expression).keys.map(unsafeValue).toSet == Set("hi", "hello"))
      assert(vea.evalGrounded(Expr(g, str("world")), Grounded.Expression).keys.map(unsafeValue).toSet == Set("hi world", "hello world"))
    }
  }

  test("preprocess") {
    {
      val pea = PreprocessEvaluationAlgorithms(simplelinear)
      import pea.eval
      given ExprMap[Int] = simplelinear

      assert(eval(Expr(a, c)) == Expr(a, c))
      assert(eval(Expr(f, c)) == Expr(a, c))
      assert(eval(Expr(g, Expr(b, c))) == c)
    }
  }

  test("rho-calculus") {
    import ProcessCalculus.*

    /**
     * recv((x, channel), square) = send(x*x, channel)
     * def square(x) = x*x  ~~>  recv((x, channel), square) = send(x*x, channel)
     *
     * recv(x, channel) = send(x, somechannel)
     *
     */

    val starta_em = ExprMap(
      Expr(recv, $, a, _1) -> 1,
      Expr(recv, $, a, Expr(send, a, _1)) -> 2,
      Expr(send, a, A) -> 10
    )

    assert(step(starta_em.keys.toSet) == Set(A, Expr(send, a, A)))
  }

  test("transform rho-calculus") {
    val send = Var(1001)
    val recv = Var(1002)

    val starta_em = ExprMap(
      Expr(recv, $, a, _1) -> 1,
      Expr(recv, $, a, Expr(send, a, _1)) -> 2,
      Expr(send, a, A) -> 10
    )

    val rs = for case App(channel, payload) <- starta_em.transform(Expr(send, $, $), Expr(_1, _2)).keys.toSet
                 result <- starta_em.transform(Expr(recv, payload, channel, $), _1).keys yield result
    assert(rs == Set(A, Expr(send, a, A)))
  }
  
  test("multivalued grounded rho-calculus") {
    import ValueEvaluationAlgorithms.pathHash

    val send = Var(1001)
    val recv = Var(1002)

    val step = Var(1004)
    val transform = Var(1005)

    given space: ExprMap[Long] = ExprMap(
      Expr(recv, $, a, _1) -> 1,
      Expr(recv, $, a, Expr(send, a, _1)) -> 2,
      Expr(send, a, A) -> 10,

      Expr(`=`, Expr(f, $, $), Expr(transform, Expr(recv, _2, _1, $), _3)) -> 19,
      Expr(`=`, step, Expr(transform, Expr(send, $, $), Expr(f, _1, _2))) -> 20
    )

    val pfs = collection.mutable.Map.empty[Int, ExprMap[Long] => ExprMap[Long]]
    var pc = 10000

    given PartialFunction[Int, ExprMap[Long] => ExprMap[Long]] = {
      case 1005 => em =>
        ExprMap.from(em.items.map((e1, s1) =>
          pc += 1
          pfs(pc) = em2 => ExprMap.from(em2.items.flatMap((e2, s2) =>
            space.transform(e1, e2).items
          ))
          Var(pc) -> 3 * s1
        ))
      case pfs(handler) => handler
    }

    assert(pathHash.evalGrounded(step, 0xc1d4f1553eecf0fL).keys.toSet == Set(A, Expr(send, a, A)))
  }

  test("templates") {
    val em = ExprMap[Int](
      Expr(f, a, b, b) -> 10,
      Expr(f, b, a, b) -> 11,
      Expr(f, b, b, a) -> 12,
      Expr(g, Expr(A, a), Expr(B, b)) -> 20,
      Expr(g, Expr(B, b), Expr(B, b)) -> 21,
      Expr(g, Expr(B, b), Expr(C, c)) -> 22,
      Expr(h, Expr(A, B, C), a) -> 30,
      Expr(h, Expr(A, B, C), b) -> 30,
    )

    // nested transform testing patterns on the data
//    em.transform(Expr($, $, $, $), )
  }

  test("nested") {

    import EvaluationAlgorithms.*
    given em: ExprMap[Unit] = ExprMap[Unit](
      Expr(`=`, Expr(f, a), a) -> (),
      Expr(`=`, Expr(f, Expr(A, $)), Expr(A, Expr(f, _1))) -> (),
    )

    val ar = Expr.nest(A, A, (List.fill(48)(A) :+ a): _*)

    for _ <- 1 to 100 do
      eval(Expr(f, ar))

    val t0 = System.nanoTime()
    eval(Expr(f, ar))
    println(System.nanoTime() - t0)
  }

  test("reduction") {
    def lookupMulti(e: Expr)(using s: ExprMap[_]): Set[Expr] =
      s.transform(Expr(`=`, e, $), _1).keys.toSet

    def lookupBackupMulti(e: Expr)(using s: ExprMap[_]): Set[Expr] =
      val nv = lookupMulti(e)
      if nv.isEmpty then Set(e)
      else nv

    def bottomUpMulti(e: Expr)(using s: ExprMap[_]): Set[Expr] =
      e.foldMap(i => lookupBackupMulti(Var(i)), (fem, aem) => fem.flatMap(f => aem.flatMap(a => lookupBackupMulti(App(f, a)))))

    def evalMulti(e: Expr)(using s: ExprMap[_]): Set[Expr] =
      fix[Set[Expr]](_.flatMap(bottomUpMulti))(Set(e))

    enum EExpr:
      case EVar(i: Int, var r: Set[EExpr])
      case EApp(f: EExpr, a: EExpr, var r: Set[EExpr])

    def prepare(e: Expr): EExpr =
      e.foldMap(
        i => EExpr.EVar(i, Set.empty),
        (f, a) => EExpr.EApp(f, a, Set.empty)
      )

//    def step(state: Set[Set[Expr]]): Set[Set[Expr]] =
//      state.map(world =>
//        world.map()
//      )

    val make_eq = Var(1000)
    val True = Var(1001)
    val False = Var(1002)
    val ite = Var(1003)
    val set_value = Var(1004)
    val `1` = Var(1005)
    val `2` = Var(1006)
    val Empty = Var(1007)

    given program: ExprMap[Int] = ExprMap(
      Expr(`=`, Expr(make_eq, $, _1), True) -> 0,

      Expr(`=`, Expr(ite, True, $, $), _1) -> 1,
      Expr(`=`, Expr(ite, False, $, $), _2) -> 2,

      Expr(`=`, Expr(set_value, `1`), True) -> 3,
      Expr(`=`, Expr(set_value, `2`), True) -> 4
    )

    val e0 = Expr(ite, Expr(make_eq, $, $),
      Expr(ite, Expr(set_value, _1), _2, Empty),
      Empty
    )

    program.addresses().foreach(println)
    println()
    program.keys.map(_.address).foreach(println)
    println(e0.address)
  }
