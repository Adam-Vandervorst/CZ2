package be.adamv.cz2

object EvaluationAlgorithms:
  import ExprExamples.*

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

  def lookup(e: Expr)(using s: ExprMap[_]): Option[Expr] =
    val es = lookupMulti(e)
    if es.size > 1 then throw RuntimeException(s"Nonlinear ${e.show}, results: ${es.map(_.show).mkString(",")}")
    else es.headOption

  def lookupBackup(e: Expr)(using s: ExprMap[_]): Expr =
    lookup(e).getOrElse(e)

  def bottomUp(e: Expr)(using s: ExprMap[_]): Expr =
    e.foldMap(i => lookupBackup(Var(i)), (f, a) => lookupBackup(App(f, a)))

  def eval(e: Expr)(using s: ExprMap[_]): Expr =
    fix[Expr](bottomUp)(e)


abstract class ValueEvaluationAlgorithms[V]:
  import ExprExamples.*

  def handleLookup(emv: V, ev: V): V
  def handleMerge(fv: V, av: V): V

  def lookupMulti(e: Expr, v: V)(using s: ExprMap[V]): ExprMap[V] =
    s.transform(Expr(`=`, e, $), _1).map(w => handleLookup(w, v))

  def lookupBackupMulti(e: Expr, v: V)(using s: ExprMap[V]): ExprMap[V] =
    val nv = lookupMulti(e, v)
    if nv.isEmpty then ExprMap(e -> v)
    else nv

  def bottomUpMulti(e: Expr, v: V)(using s: ExprMap[V]): ExprMap[V] =
    e.foldMap(i => lookupBackupMulti(Var(i), v), (fem, aem) => ExprMap.from(fem.items.flatMap((f, fv) => aem.items.flatMap((a, av) => lookupBackupMulti(App(f, a), handleMerge(fv, av)).items))))

  def evalMulti(e: Expr, v: V)(using s: ExprMap[V]): ExprMap[V] =
    fixproject[ExprMap[V], Set[Expr]](em => ExprMap.from(em.items.flatMap(bottomUpMulti(_, _).items)), _.keys.toSet)(ExprMap[V](e -> v))

  def lookup(e: Expr, v: V)(using s: ExprMap[V]): Option[(Expr, V)] =
    val es = s.transform(Expr(`=`, e, $), _1)
    if es.size > 1 then throw RuntimeException(s"Nonlinear ${e.show}, results: ${es.keys.map(_.show).mkString(",")}")
    else es.items.headOption.map((e, w) => e -> handleLookup(w, v))

  def lookupBackup(e: Expr, v: V)(using s: ExprMap[V]): (Expr, V) =
    lookup(e, v).getOrElse(e -> v)

  def bottomUp(e: Expr, v: V)(using s: ExprMap[V]): (Expr, V) =
    e.foldMap(i => lookupBackup(Var(i), v), {case ((f, fv), (a, av)) => lookupBackup(App(f, a), handleMerge(fv, av))})

  def eval(e: Expr, v: V)(using s: ExprMap[V]): (Expr, V) =
    fixproject[(Expr, V), Expr](bottomUp, _._1)(e -> v)

  def apply(e: Expr)(using s: ExprMap[V]): ExprMap[V]

  def unapply(e: Expr)(using s: ExprMap[V]): Option[(Expr, V)]


object ValueEvaluationAlgorithms:
  val graphvizDebug: ValueEvaluationAlgorithms[Int] = new:
    def handleLookup(emv: Int, ev: Int): Int =
      val r = 100 + util.Random.nextInt(9900)
      println(s"$r [label=\"lookup $emv\"]")
      println(s"$ev -> $r [label=$ev]")
      r

    def handleMerge(fv: Int, av: Int): Int =
      val r = 100 + util.Random.nextInt(9900)
      println(s"$r [label=\"merge\"]")
      println(s"$fv -> $r [label=$fv]")
      println(s"$av -> $r [label=$av]")
      r

    def apply(e: Expr)(using s: ExprMap[Int]): ExprMap[Int] =
      evalMulti(e, 10000)

    def unapply(e: Expr)(using s: ExprMap[Int]): Option[(Expr, Int)] =
      scala.util.Try(eval(e, 10000)).toOption


  val textDebug: ValueEvaluationAlgorithms[String] = new:
    private var c = 0

    def handleLookup(emv: String, ev: String): String =
      val r = toLetters(c)
      c += 1
      println(s"lookup  ${r.padTo(4, ' ')} ${emv.padTo(6, ' ')} ${ev.padTo(6, ' ')}")
      r

    def handleMerge(fv: String, av: String): String =
      val r = toLetters(c)
      c += 1
      println(s"merge   ${r.padTo(4, ' ')} ${fv.padTo(6, ' ')} ${av.padTo(6, ' ')}")
      r

    def apply(e: Expr)(using s: ExprMap[String]): ExprMap[String] =
      c = 0
      evalMulti(e, "init")

    def unapply(e: Expr)(using s: ExprMap[String]): Option[(Expr, String)] =
      c = 0
      scala.util.Try(eval(e, "init")).toOption


  val pathHash: ValueEvaluationAlgorithms[Long] = new:
    private var c = 3

    def handleLookup(emv: Long, ev: Long): Long =
      c += 2
      emv*c ^ ev
    def handleMerge(fv: Long, av: Long): Long =
      fv*3 + av

    def apply(e: Expr)(using s: ExprMap[Long]): ExprMap[Long] =
      c = 3
      evalMulti(e, 0xc1d4f1553eecf0fL)

    def unapply(e: Expr)(using s: ExprMap[Long]): Option[(Expr, Long)] =
      c = 3
      scala.util.Try(eval(e, 0xc1d4f1553eecf0fL)).toOption
end ValueEvaluationAlgorithms
