package be.adamv.cz2

import munit.FunSuite

class EvaluationTest extends FunSuite:
  import ExprExamples.*
  import ExprMapExamples.*

  test("evaluation") {
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

  test("multivalued evaluation") {
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

  test("traced evaluation") {
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

  test("grounded evaluation") {
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


      assert(pathHash.evalGrounded(f, 0xc1d4f1553eecf0fL).keys.map{ case Var(camap(s)) => s }.toSet == Set("<>"))
      assert(pathHash.evalGrounded(greeting, 0xc1d4f1553eecf0fL).keys.map{ case Var(camap(s)) => s }.toSet == Set("hi", "hello"))
      assert(pathHash.evalGrounded(Expr(g, str("world")), 0xc1d4f1553eecf0fL).keys.map{ case Var(camap(s)) => s }.toSet == Set("hi world", "hello world"))
    }
    {
      // counted array
    }
    {
      // stored in EM
    }
  }

  test("preprocess evaluation") {
    {
      val pea = PreprocessEvaluationAlgorithms(simplelinear)
      import pea.eval
      given ExprMap[Int] = simplelinear

      assert(eval(Expr(a, c)) == Expr(a, c))
      assert(eval(Expr(f, c)) == Expr(a, c))
      assert(eval(Expr(g, Expr(b, c))) == c)
    }
  }
