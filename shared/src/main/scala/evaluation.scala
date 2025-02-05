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

object ProcessCalculus:
  val send = Var(1001)
  val recv = Var(1002)

  def step(em: Set[Expr]): Set[Expr] =
    for case Expr(`send`, channel, payload) <- em
        case Expr(`recv`, pattern, `channel`, body) <- em
        case (l, r) <- payload matches pattern
    yield body.substRel(r)


// H | H | O | Cl |
// {H; H; O} => {H2O} | {H; H; O} => {H2O} | {H; Cl} => {HCl}

// 1/3 times: {HCl; H; O; {H; H; O} => {H2O}; {H; H; O} => {H2O}}
// 2/3 times: {H2O; Cl; {H; H; O} => {H2O}; {H; Cl} => {HCl}}

//noinspection NonAsciiCharacters
//class MeTTaCalculus:
//  import MeTTaCalculus.{==>, ∅}
//  val state = ExprMap[ExprMap[Long]]()
//  def add(e: Expr, n: Long): Long = e match
//    case `∅` => ()
//    case App(App(lhs, `==>`), rhs) =>
//      val possible = state.indiscriminateBidirectionalMatching(lhs)
//      possible.foreachItem((k, conts) => {
//        lhs.matches(k) match
//          case Some((ab, sb)) =>
//            val rrhs = rhs.substReIndex(ab)
//            conts.foreachItem((c, m) => {
//              val interactions = n min m
//              add(rrhs, interactions)
//              add(c.substReIndex(sb), interactions)
//            })
//          case None => ()
//      })
//  state.updateWithDefault(nlhs)(ExprMap(nrhs -> 1L))(x => {x.updateWithDefault(nrhs)(1L)(_ + 1); x})
//    case nlhs =>
//      add(App(App(nlhs, `==>`), ∅))
//      state.updateWithDefault(nlhs)(ExprMap(∅ -> 1L))(x => {x.updateWithDefault(∅)(1L)(_ + 1); x})


object MeTTaCalculus:
  val ==> = Var(1001)
  val ∅ = Var(1002)


class PreprocessEvaluationAlgorithms(s: ExprMap[_]):
  import ExprExamples.*

//  def matcher(m: Expr, d: Expr, stack: Seq[Expr]): Option[Seq[Expr]] = (m, d) match
//    case (Var(i), )

  val subs_fs: Map[Int, Expr] = s.keys.collect{ case Expr(`=`, App(Var(f), Var(0)), rhs) => f -> rhs }.toMap
  val matches_fs: Map[Int, (Expr, Expr)] = s.keys.collect{ case Expr(`=`, App(Var(f), args), body) => f -> (args, body) }.toMap

  def bottomUp(e: Expr): Expr =
    e.foldMap(i => Var(i), {
      case (Var(subs_fs(rhs)), a) => rhs.substRel(Seq(a))
      case (Var(matches_fs(args, body)), a) if (args matches a).nonEmpty => body.substRel((args matches a).get._1)
      case (f, a) => App(f, a)
    })

  def eval(e: Expr): Expr =
    fix[Expr](bottomUp)(e)


abstract class ValueEvaluationAlgorithms[V]:
  import ExprExamples.*

  def handleLookup(emv: V, ev: V): V
  def handleMerge(fv: V, av: V): V

  def lookupMulti(e: Expr, v: V)(using s: ExprMap[V]): ExprMap[V] =
    val r = s.transform(Expr(`=`, e, $), Var(-e.nvarsN - 1))
    r.map(w => handleLookup(w, v))

  def lookupBackupMulti(e: Expr, v: V)(using s: ExprMap[V]): ExprMap[V] =
    val nv = lookupMulti(e, v)
    if nv.isEmpty then ExprMap(e -> v)
    else nv

  def bottomUpMulti(e: Expr, v: V)(using s: ExprMap[V]): ExprMap[V] =
    e.foldMap(i => lookupBackupMulti(Var(i), v), (fem, aem) => ExprMap.from(fem.items.flatMap((f, fv) => aem.items.flatMap((a, av) => lookupBackupMulti(App(f, a), handleMerge(fv, av)).items))))

  def evalMulti(e: Expr, v: V)(using s: ExprMap[V]): ExprMap[V] =
    fixproject[ExprMap[V], Set[Expr]](em => ExprMap.from(em.items.flatMap(bottomUpMulti(_, _).items)), _.keys.toSet)(ExprMap[V](e -> v))

  def lookup(e: Expr, v: V)(using s: ExprMap[V]): Option[(Expr, V)] =
    val es = lookupMulti(e, v)
    if es.size > 1 then throw RuntimeException(s"Nonlinear ${e.show}, results: ${es.keys.map(_.show).mkString(",")}")
    else es.items.headOption

  def lookupBackup(e: Expr, v: V)(using s: ExprMap[V]): (Expr, V) =
    lookup(e, v).getOrElse(e -> v)

  def bottomUp(e: Expr, v: V)(using s: ExprMap[V]): (Expr, V) =
    e.foldMap(i => lookupBackup(Var(i), v), {case ((f, fv), (a, av)) => lookupBackup(App(f, a), handleMerge(fv, av))})

  def eval(e: Expr, v: V)(using s: ExprMap[V]): (Expr, V) =
    fixproject[(Expr, V), Expr](bottomUp, _._1)(e -> v)

  def bottomUpMultiGrounded(e: Expr, v: V)(using s: ExprMap[V], g: PartialFunction[Int, ExprMap[V] => ExprMap[V]]): ExprMap[V] =
    var newv = 0
    var doeval = true
    e.foldMap(i =>
      if i > 0 then lookupBackupMulti(Var(i), v)
      else
        val r = ExprMap(Var(i) -> v)
        if i == 0 then newv += 1
        if i < -newv then doeval = false
        r
      ,
      (fem, aem) =>
      ExprMap.from(
        fem.items.flatMap((f, fv) => f match
          case Var(g(grounded)) =>
            grounded(aem).items
          case _ =>
            aem.items.flatMap((a, av) =>
              if doeval then lookupBackupMulti(App(f, a), handleMerge(fv, av)).items
              else List(App(f, a) -> handleMerge(fv, av)))
        )
      )
    )

  def evalGrounded(em: ExprMap[V])(using s: ExprMap[V], g: PartialFunction[Int, ExprMap[V] => ExprMap[V]]): ExprMap[V] =
    fixproject[ExprMap[V], Set[Expr]](em => ExprMap.from(em.items.flatMap(bottomUpMultiGrounded(_, _).items)), _.keys.toSet)(em)

  def evalGrounded(e: Expr, v: V)(using s: ExprMap[V], g: PartialFunction[Int, ExprMap[V] => ExprMap[V]]): ExprMap[V] =
    evalGrounded(ExprMap[V](e -> v))

  def evalGroundedN(e: Expr, v: V, N: Int)(using s: ExprMap[V], g: PartialFunction[Int, ExprMap[V] => ExprMap[V]]): ExprMap[V] =
    var s = ExprMap[V](e -> v)
    for _ <- 1 to N do
      s = ExprMap.from(s.items.flatMap(bottomUpMultiGrounded(_, _).items))
    s

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

  def neqRaise[A](initial: A): ValueEvaluationAlgorithms[A] = new:
    def handleLookup(emv: A, ev: A): A = emv

    def handleMerge(fv: A, av: A): A = if fv == av then fv else throw RuntimeException(f"$fv != $av in merge")

    def apply(e: Expr)(using s: ExprMap[A]): ExprMap[A] =
      evalMulti(e, initial)

    def unapply(e: Expr)(using s: ExprMap[A]): Option[(Expr, A)] =
      scala.util.Try(eval(e, initial)).toOption

  def ignore[A]: ValueEvaluationAlgorithms[A] = new:
    def handleLookup(emv: A, ev: A): A = emv
  
    def handleMerge(fv: A, av: A): A = fv
  
    def apply(e: Expr)(using s: ExprMap[A]): ExprMap[A] =
      evalMulti(e, null.asInstanceOf[A])
  
    def unapply(e: Expr)(using s: ExprMap[A]): Option[(Expr, A)] =
      scala.util.Try(eval(e, null.asInstanceOf[A])).toOption
end ValueEvaluationAlgorithms
