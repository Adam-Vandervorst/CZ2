import be.adamv.cz2.*
import ExprExamples.{$, _1}


@main def comparison =
  // the goal is to get this to 10_000, which works on Scala Native!
  val max_number = 1000
  val numbers = max_number * 2

  val S = Expr.Var(1)
  val Z = Expr.Var(2)

  val em = ExprMap[Int]()

  def wrap(e: Expr, depth: Int): Expr =
    if depth == 0 then e else wrap(Expr.App(S, e), depth - 1)

  for i <- 0 until numbers do
    em.update(wrap(Z, scala.util.Random.nextInt(max_number)), i)

  println(s"final size ${em.size}")

  for match_fraction <- Seq(0.02, 0.1, 0.5, 0.9, 0.98) do
    println(s"match fraction $match_fraction")
    val filter_up_to = (max_number * (1 - match_fraction)).toInt

    val instr_t0 = System.nanoTime()
    println("instr" -> em.execute(Seq.fill(filter_up_to)(Instr.Unapply(1)) ++ Seq.fill(filter_up_to)(Instr.Apply(1))).size)
    println(System.nanoTime() - instr_t0)
    // JVM: 4_624_331, 7_128_673, 4_274_669, 3_324_052, 2_430_492
    // SN: 693_105, 657_601, 288_301, 226_823, 336_549

    val indmatch_t0 = System.nanoTime() // doesn't actually do the full computation
    println("indmatch" -> em.indiscriminateBidirectionalMatching(wrap($, filter_up_to)).size)
    println(System.nanoTime() - indmatch_t0)
    // JVM: 1_682_786, 3_592_337, 1_602_771, 1_269_425, 1_095_633
    // SN: 1_015_691, 1_788_856, 303_115, 201_913, 322_822

    val match_t0 = System.nanoTime()
    println("match" -> em.transformMatches(wrap($, filter_up_to), wrap(_1, filter_up_to)).size)
    println(System.nanoTime() - match_t0)
    // JVM: 11_852_597, 28_173_947, 78_252_640, 110_398_690, 106_115_938
    // SN: 5_182_987, 11_004_192, 41_916_771, 70_540_595, 69_095_555

//    //     overkill functionality anyways
//    val unif_t0 = System.nanoTime()
//    println("unif" -> em.transform(wrap($, filter_up_to), wrap(_1, filter_up_to)).size)
//    println(System.nanoTime() - unif_t0)
//    // JVM: 28_210_305_663, 120_568_613_892, 263_571_825_084, 164_868_223_743, 150_999_877_520
//    // SN: 31_575_608_451, 115_003_094_034, 257_809_221_022, 168_504_966_827, 150_004_214_467
