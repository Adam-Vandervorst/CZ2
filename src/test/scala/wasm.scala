package be.adamv.cz2

import be.adamv.cz2.ExprExamples.{$x, $y}
import munit.FunSuite


class WASMTest extends FunSuite:
  import ExprExamples.{`=`, _1, _2, _3, $, f, g, h}
  
  val top = new RangeStorage(100_000, Int.MaxValue)
  val commands = Set("func", "param", "local", "result", "export", "import", "module", "call", "loop", "br_if", "return", "type", "table", "elem", "call_indirect")
  val mods = Set("get", "const")
  val types = Set("i32", "i64", "f32", "f64", "funcref")
  val ops = Set("add", "mul", "sub", "gt_u")
  val names = (commands union mods union types union ops)
  val ns = top.storeInRange(names)

  extension (inline sc: StringContext)
    inline def v(inline args: Any*): Expr =
      Var(ns.lookup(StringContext.standardInterpolator(identity, args, sc.parts)).get)

  val strs = top.inRange[String](1000)
  val i32s = top.inRange[Int](1000)
  val i64s = top.inRange[Long](1000)

  def str(s: String): Expr = Var(strs.add(s))
  def i32(i: Int): Expr = Var(i32s.add(i))
  def i64(i: Long): Expr = Var(i64s.add(i))

  /*
  (func (param i32) (param f32) (local f64)
    local.get 0
    local.get 1
    local.get 2)
  */
//  val f1 = Expr(func, Expr(param, i32), Expr(param, f32), Expr(local, f64),
//    Expr(local, get, _1),
//    Expr(local, get, _2),
//    Expr(local, get, _3),
//  )

  /*
  (func (param $p i32)
    (result i32)
    local.get $p
    local.get $p
    i32.add)
  */
//  val f2 = Expr(func, Expr(param, i32), Expr(result, i32),
//    Expr(local, get, _1),
//    Expr(local, get, _1),
//    Expr(i32, add),
//  )

  /*
  (module
    (func $add (param $lhs i32) (param $rhs i32) (result i32)
      local.get $lhs
      local.get $rhs
      i32.add)
    (export "add" (func $add))
  )
  */
//  val m1 = Expr(module,
//    Expr(func, Var(100), Expr(param, i32), Expr(param, i32), Expr(result, i32),
//      Expr(local, get, _1),
//      Expr(local, get, _2),
//      Expr(i32, add),
//    ),
//    Expr(`export`, str("add"), Expr(func, Var(100)))
//  )

  /*
  (module
    (func $getAnswer (result i32)
      i32.const 42)
    (func (export "getAnswerPlus1") (result i32)
      call $getAnswer
      i32.const 1
      i32.add))
  */
//  val m2 = Expr(module,
//    Expr(func, Var(100), Expr(result, i32),
//      Expr(i32, const, i32(42))),
//    Expr(func, Expr(`export`, str("getAnswerPlus1")), Expr(result, i32),
//      Expr(call, Var(100)),
//      Expr(i32, const, i32(1)),
//      Expr(i32, add))
//  )

  /*
  (module
    (import "console" "log" (func $log (param i32)))
    (func (export "logIt")
      i32.const 13
      call $log))
  */
//  val m3 = Expr(module,
//    Expr(`import`, str("console"), str("log"), Expr(func, Var(100), Expr(param, i32))),
//    Expr(func, Expr(`export`, str("logIt")),
//      Expr(i32, const, i32(13)),
//      Expr(call, Var(100)))
//  )

  /*
  (module
  (func $pick0 (param i64) (result i64 i64)
    (local.get 0) (local.get 0)
  )

  (func $pick1 (param i64 i64) (result i64 i64 i64)
    (local.get 0) (local.get 1) (local.get 0)
  )

  (func $fac (param i64) (result i64)
    (i64.const 1) (local.get 0)
    (loop $l (param i64 i64) (result i64)
      (call $pick1) (call $pick1) (i64.mul)
      (call $pick1) (i64.const 1) (i64.sub)
      (call $pick0) (i64.const 0) (i64.gt_u)
      (br_if $l)
      (call $pick1) (return)
    )
  )
  (export "fac" (func $fac))
  )
  */
  val fac_em = ExprMap(
    Expr(v"func", f, Expr(v"param", v"i64"), Expr(v"result", v"i64", v"i64"),
      Expr(v"local", v"get", _1), Expr(v"local", v"get", _1)
    ) -> 0,

    Expr(v"func", g, Expr(v"param", v"i64", v"i64"), Expr(v"result", v"i64", v"i64", v"i64"),
      Expr(v"local", v"get", _1), Expr(v"local", v"get", _2), Expr(v"local", v"get", _1)
    ) -> 1,

    Expr(v"func", h, Expr(v"param", v"i64"), Expr(v"result", v"i64"),
      Expr(v"i64", v"const", i64(1)), Expr(v"local", v"get", _1),
      Expr(v"loop", Expr(v"param", v"i64", v"i64"), Expr(v"result", v"i64"),
        Expr(v"call", g), Expr(v"call", g), Expr(v"i64", v"mul"),
        Expr(v"call", g), Expr(v"i64", v"const", i64(1)), Expr(v"i64", v"sub"),
        Expr(v"call", f), Expr(v"i64", v"const", i64(0)), Expr(v"i64", v"gt_u"),
        Expr(v"br_if", i32(0)),
        Expr(v"call", g), v"return"
      )
    ) -> 2,

    Expr(v"export", str("fac"), Expr(v"func", h)) -> 3
  )
  /*
  (module
    (table 2 funcref)
    (func $f1 (result i32)
      i32.const 42)
    (func $f2 (result i32)
      i32.const 13)
    (elem (i32.const 0) $f1 $f2)
    (type $return_i32 (func (result i32)))
    (func (export "callByIndex") (param $i i32) (result i32)
      local.get $i
      call_indirect (type $return_i32))
  )
  */

  val table_em = ExprMap(
    Expr(v"table", i32(2), v"funcref") -> 0,
    Expr(v"func", f, Expr(v"result", v"i32"), Expr(v"i32", v"const", i32(42))) -> 1,
    Expr(v"func", g, Expr(v"result", v"i32"), Expr(v"i32", v"const", i32(13))) -> 2,
    Expr(v"elem", Expr(v"i32", v"const", i32(0)), f, g) -> 3,
    Expr(v"type", h, Expr(v"func", Expr(v"result", v"i32"))) -> 4,
    Expr(v"func", Expr(v"export", str("callByIndex")), Expr(v"param", v"i32"), Expr(v"result", v"i32"),
      Expr(v"local", v"get", _1), Expr(v"call_indirect", Expr(v"type", h))
    ) -> 5,
  )

  def exprToWat(e: Expr): String = e.foldMapAssoc(i =>
    if ns.couldContain(i) then ns.get(i).get
    else if strs.couldContain(i) then "\"" + strs.get(i).get + "\""
    else if i32s.couldContain(i) then i32s.get(i).get.toString
    else if i64s.couldContain(i) then i64s.get(i).get.toString
    else if i < 0 then (~i).toString
    else "$X" + i.toString, {
    case Seq(a, b, r: _*) if mods(b) || ops(b) => ((a + "." + b) +: r).mkString("(", " ", ")")
    case r => r.mkString("(", " ", ")")
  })

  def toWat(em: ExprMap[_]): String =
    "(module\n" +
      em.keys.map(exprToWat).mkString("\n")
    + "\n)"

  test("basic toWat") {
    println(toWat(table_em))
    println(toWat(fac_em))
  }
