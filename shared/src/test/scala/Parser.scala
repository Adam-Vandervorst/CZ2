package be.adamv.cz2

import munit.FunSuite


class ParserTest extends FunSuite:
  import ExprExamples.{f, g, h, a, b, c, `=`, `,`, e1, e2, e3, r1}

  val names = GrowableRangeStorage[String](-100, 100)
  val V = 10
  names.importMap(Map(
    f.leftMost -> "f", g.leftMost -> "g", h.leftMost -> "h",
    a.leftMost -> "a", b.leftMost -> "b", c.leftMost -> "c",
    `=`.leftMost -> "=", `,`.leftMost -> ",",
    -V -> "$x", -V - 1 -> "$y", -V - 2 -> "$z"))

  val PP = NamedPrettyPrinter(names)


  test("named printer") {
    assert(PP.sexpression(e1.toAbsolute(V), colored = false) == "(f $x a)")
    assert(PP.sexpression(e2.toAbsolute(V), colored = false) == "(f $x $y $y $x)")
    assert(PP.sexpression(e3.toAbsolute(V), colored = false) == "(f $x (g $x $y))")
    assert(PP.sexpression(r1.toAbsolute(V), colored = false) == "(= (f (, $x (g (g $y)))) (h (, $x (g a)) (, $x (g $y))))")
  }

  test("basic") {
//    names

//    assert(parse("// test comment\n/*block\nmultiline*/") == Vector(
//      CommentExpr("test comment", false),
//      CommentExpr("block\nmultiline", true)))
//    assert(parse("(a b)") == Vector(AddExpr(App(Concept("a"), Concept("b")))))
//    assert(parse("x") == Vector(AddExpr(Concept("x"))))
//    assert(parse("(a (b c))") == Vector(AddExpr(App(Concept("a"), App(Concept("b"), Concept("c"))))))
//    assert(parse("(a b c)") == Vector(AddExpr(App(App(Concept("a"), Concept("b")), Concept("c")))))
//    assert(parse("// a test\n(eq (succ $n) (S $n))\n!(println (succ Z))") == Vector(
//      CommentExpr("a test", false),
//      AddExpr(App(App(Concept("eq"), App(Concept("succ"), Var("n"))), App(Concept("S"), Var("n")))),
//      ExecuteExpr(App(Concept("println"), App(Concept("succ"), Concept("Z"))))))
//    assert(parse("\na\n\nb\n") == Vector(AddExpr(Concept("a")), AddExpr(Concept("b"))))
//    assert(parse("(a\n (s b)\n)") == Vector(AddExpr(App(Concept("a"), App(Concept("s"), Concept("b"))))))
  }
