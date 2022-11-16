package be.adamv.cz2

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

  def apply(e: Expr)(using s: ExprMap[V]): ExprMap[V]

object ValueEvaluationAlgorithms:
  val graphvizDebug: ValueEvaluationAlgorithms[Int] = new:
    def lookup(emv: Int, ev: Int): Int =
      val r = 100 + util.Random.nextInt(9900)
      println(s"$r [label=\"lookup $emv\"]")
      println(s"$ev -> $r [label=$ev]")
      r

    def merge(fv: Int, av: Int): Int =
      val r = 100 + util.Random.nextInt(9900)
      println(s"$r [label=\"merge\"]")
      println(s"$fv -> $r [label=$fv]")
      println(s"$av -> $r [label=$av]")
      r

    def apply(e: Expr)(using s: ExprMap[Int]): ExprMap[Int] = allpossible(e, 10000)


  val textDebug: ValueEvaluationAlgorithms[String] = new:
    private var c = 0

    def lookup(emv: String, ev: String): String =
      val r = toLetters(c)
      c += 1
      println(s"lookup  ${r.padTo(4, ' ')} ${emv.padTo(6, ' ')} ${ev.padTo(6, ' ')}")
      r

    def merge(fv: String, av: String): String =
      val r = toLetters(c)
      c += 1
      println(s"merge   ${r.padTo(4, ' ')} ${fv.padTo(6, ' ')} ${av.padTo(6, ' ')}")
      r

    def apply(e: Expr)(using s: ExprMap[String]): ExprMap[String] =
      c = 0
      allpossible(e, "init")

  val pathHash: ValueEvaluationAlgorithms[Long] = new:
    private var c = 3

    def lookup(emv: Long, ev: Long): Long =
      c += 2
      emv*c ^ ev
    def merge(fv: Long, av: Long): Long =
      fv*3 + av

    def apply(e: Expr)(using s: ExprMap[Long]): ExprMap[Long] =
      c = 3
      allpossible(e, 0xc1d4f1553eecf0fL)

end ValueEvaluationAlgorithms
