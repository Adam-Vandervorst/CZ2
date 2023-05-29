import be.adamv.cz2.*
import scala.annotation.tailrec
import ExprExamples.{$, _1}


@main def comparison =
  val max_depth = 1_000
  val numbers = max_depth * 2

  val S = Expr.Var(1)
  val Z = Expr.Var(2)

  @tailrec def wrap(e: Expr, depth: Int): Expr =
    if depth == 0 then e else wrap(Expr.App(S, e), depth - 1)

  val em = ExprMap[Int]()

  val constr_t0 = System.nanoTime()
  for i <- 0 until numbers do
    em.update(wrap(Z, scala.util.Random.nextInt(max_depth)), i)
  println(s"construction ${System.nanoTime() - constr_t0}")
  // JS  184459508
  // JS  144228553  (warm)
  // JVM 195757978
  // JVM  35621060  (warm)
  // SN   83199766

  println(s"final size: ${em.size}")

  for match_fraction <- Seq(0.02, 0.1, 0.5, 0.9, 0.98) do
    println(s"match fraction: $match_fraction")
    val filter_up_to = (max_depth * (1 - match_fraction)).toInt

    val instr_t0 = System.nanoTime()
    val instr_res = em.execute(Seq.fill(filter_up_to)(Instr.Unapply(1)) ++ Seq.fill(filter_up_to)(Instr.Apply(1))).size
    println(s"instr ${System.nanoTime() - instr_t0}")
    // JS  6299522, 2201821, 983000, 380490, 360935
    // JS   641336,  685470, 566153, 311795, 259502  (warm)
    // JVM 9006575, 3575982, 681426, 346279, 251953
    // JVM 1481764,  430652, 280075, 230496, 192712  (warm)
    // SN   536610,  205297, 187590, 144073, 172864

    val indmatch_t0 = System.nanoTime() // doesn't actually do the full computation
    val indmatch_res = em.indiscriminateBidirectionalMatching(wrap($, filter_up_to)).size
    println(s"indmatch ${System.nanoTime() - indmatch_t0}")
    // JS  17797253, 4405912, 605865, 239891, 261179
    // JS    942064, 1046903, 767254, 294844, 206218  (warm)
    // JVM 20335562, 1303147, 660638, 243406, 158735
    // JVM   339960,  290298, 224865, 118426,  99283  (warm)
    // SN    860037,  325911, 215328, 103786, 134431

    val match_t0 = System.nanoTime()
    val match_res = em.transformMatches(wrap($, filter_up_to), wrap(_1, filter_up_to)).size
    println(s"match ${System.nanoTime() - match_t0}")
    // JS  51024865, 53177841, 114906564, 113302728, 104342177
    // JS  10963463, 41443489,  97003249, 107078830,  92826285  (warm)
    // JVM 27399337, 19459809,  49566962,  65825190,  63946557
    // JVM  1926968,  7001827,  31111352,  48823073,  49982786  (warm)
    // SN   5511441,  9394163,  41877678,  68738905,  70053760

    assert(instr_res == indmatch_res)
    assert(indmatch_res == match_res)
