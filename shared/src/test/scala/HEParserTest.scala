package be.adamv.cz2
import be.adamv.cz2.ExprExamples.{$, _1}
import munit.*




class SExprParserTests extends FunSuite {
  def parseAtoms(program: String): (List[Expr], RangeStorage[String]) = {
    val storage = RangeStorage.highPos[String]()
    val parser = new Parser:
       override val empty: Int = 10000
       override val singleton: Int = 10001
       override def tokenizer(s: String): Option[Expr] = Some(storage.addV(s))

    val it = program.iterator.buffered
    var result = List.empty[Expr]
    var last = parser.sexpr(it)
    while last.isDefined do
      result = result :+ last.get
      last = parser.sexpr(it)
    result -> storage
  }

  test("text_var") {
    assertEquals(parseAtoms("$n")._1, List(Var(0)))
  }

  test("text_sym") {
    val (result, dict) = parseAtoms("test")
    result match
      case List(Var(dict.indexToValue(v))) if v == "test" => ()
      case _ => fail("Not matching")
  }

  test("text_quoted_string") {
    val (result, dict) = parseAtoms("\"te st\"")
    result match
      case List(Var(dict.indexToValue(v))) if v == "\"te st\"" => ()
      case _ => fail(f"Not matching ${result} ${dict.indexToValue}")
  }

  test("text_recognize_full_token") {
    val it = "ab".iterator.buffered
    val parser = new Parser:
      override val empty: Int = 10000
      override val singleton: Int = 10001
      override def tokenizer(s: String): Option[Expr] = Some(Expr.Var(if s == "b" then 2 else 1))

    assertEquals(parser.sexpr(it), Some(Var(1)))
    assertEquals(parser.sexpr(it), None)
  }

  test("text_gnd") {
    val it = "(3d 42)".iterator.buffered
    val parser = new Parser:
      override val empty: Int = 10000
      override val singleton: Int = 10001
      override def tokenizer(s: String): Option[Expr] = s.toIntOption match
        case Some(i) => Some(Expr.Var(i))
        case None => Some(Expr.Var(1))

    assertEquals(parser.sexpr(it), Some(App(Expr.Var(1), Expr.Var(42))))
    assertEquals(parser.sexpr(it), None)
  }

  test("text_expr") {
    val (result, dict) = parseAtoms("(= (fac $n) (* $n (fac (- $n 1))))")
    import dict.v
    assertEquals(result.head, Expr(v"=", Expr(v"fac", $), Expr(v"*", _1, Expr(v"fac", Expr(v"-", _1, v"1")))))
  }

  test("text_few_expr") {
    val (result, dict) = parseAtoms("(a) (b)")
    import dict.v
    assertEquals(result, List(Expr(Var(10001), v"a"), Expr(Var(10001), v"b")))
  }

  test("panic_on_unbalanced_brackets") {
    intercept[RuntimeException] {
      parseAtoms("(a))")
    }
  }

  test("comment_base") {
    val (result, dict) = parseAtoms(";(a 4)\n      (b 5)")
    import dict.v
    assertEquals(result, List(Expr(v"b", v"5")))
  }

  test("comment_in_sexpr") {
    val (result, dict) = parseAtoms(" (a ; 4)\n    5)")
    import dict.v
    assertEquals(result, List(Expr(v"a", v"5")))
  }

  test("comment_endl") {
    val (result, dict) = parseAtoms(" (a 4);\n      (b 5)")
    import dict.v
    assertEquals(result, List(Expr(v"a", v"4"), Expr(v"b", v"5")))
  }

  test("exprs") {
    {
      val (result, dict) = parseAtoms("(f $x a)")
      import dict.v
      assertEquals(result, List(Expr(v"f", $, v"a")))
    }
    {
      val (result, dict) = parseAtoms("(a b)")
      import dict.v
      assertEquals(result, List(Expr(v"a", v"b")))
    }
    {
      val (result, dict) = parseAtoms("(s)")
      import dict.v
      assertEquals(result, List(Expr(Var(10001), v"s")))
    }
    {
      val (result, dict) = parseAtoms("()")
      assertEquals(result, List(Var(10000)))
    }
    {
      val (result, dict) = parseAtoms("(a (b c))")
      import dict.v
      assertEquals(result, List(Expr(v"a", Expr(v"b", v"c"))))
    }
    {
      val (result, dict) = parseAtoms("; a test\n(eq (succ $n) (S $n))\n(println! (succ Z))")
      import dict.v
      assertEquals(result, List(Expr(v"eq", Expr(v"succ", $), Expr(v"S", _1)), Expr(v"println!", Expr(v"succ", v"Z"))))
    }
    {
      val (result, dict) = parseAtoms("\na\n\nb\n")
      import dict.v
      assertEquals(result, List(v"a", v"b"))
    }
    {
      val (result, dict) = parseAtoms("(a\n (s b)\n)")
      import dict.v
      assertEquals(result, List(Expr(v"a", Expr(v"s", v"b"))))
    }
  }
}