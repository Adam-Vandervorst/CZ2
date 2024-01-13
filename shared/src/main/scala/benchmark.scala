import be.adamv.cz2.*

import scala.io.Source
import scala.util.{Random, Using}
import scala.annotation.tailrec
import scala.collection.mutable
import ExprExamples.{$, _1}
import be.adamv.cz2.Instr.Unapply


def comparison =
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

  val fs = for match_fraction <- Seq(0.02, 0.1, 0.5, 0.9, 0.98) yield
    println(s"match fraction: $match_fraction")
    val filter_up_to = (max_depth * (1 - match_fraction)).toInt

    val instr_t0 = System.nanoTime()
    val instr_res = em.execute(Seq.fill(filter_up_to)(Instr.Unapply(1)) ++ Seq.fill(filter_up_to)(Instr.Apply(1)))
    val instr_cnt = instr_res.size
    println(s"instr ${System.nanoTime() - instr_t0}")
    // JS  6299522, 2201821, 983000, 380490, 360935
    // JS   641336,  685470, 566153, 311795, 259502  (warm)
    // JVM 9006575, 3575982, 681426, 346279, 251953
    // JVM 1481764,  430652, 280075, 230496, 192712  (warm)
    // SN   536610,  205297, 187590, 144073, 172864

    val indmatch_t0 = System.nanoTime() // doesn't actually do the full computation
    val indmatch_cnt = em.indiscriminateBidirectionalMatching(wrap($, filter_up_to)).size
    println(s"indmatch ${System.nanoTime() - indmatch_t0}")
    // JS  17797253, 4405912, 605865, 239891, 261179
    // JS    942064, 1046903, 767254, 294844, 206218  (warm)
    // JVM 20335562, 1303147, 660638, 243406, 158735
    // JVM   339960,  290298, 224865, 118426,  99283  (warm)
    // SN    860037,  325911, 215328, 103786, 134431

    val match_t0 = System.nanoTime()
    val match_cnt = em.transformMatches(wrap($, filter_up_to), wrap(_1, filter_up_to)).size
    println(s"match ${System.nanoTime() - match_t0}")
    // JS  51024865, 53177841, 114906564, 113302728, 104342177
    // JS  10963463, 41443489,  97003249, 107078830,  92826285  (warm)
    // JVM 27399337, 19459809,  49566962,  65825190,  63946557
    // JVM  1926968,  7001827,  31111352,  48823073,  49982786  (warm)
    // SN   5511441,  9394163,  41877678,  68738905,  70053760

    assert(instr_cnt == indmatch_cnt)
    assert(indmatch_cnt == match_cnt)

    instr_res

  val joins_t0 = System.nanoTime()
  for (f, i) <- fs.zipWithIndex do
    for f_ <- fs.take(i) do
      assert(f.union(f_) == f)
  println(s"join ${System.nanoTime() - joins_t0}")
  // JS  66323146
  // JS  19702789  (warm)
  // JVM 67014622
  // JVM  6653459  (warm)
  // SN   6837690

  val meets_t0 = System.nanoTime()
  for (f, i) <- fs.zipWithIndex do
    for f_ <- fs.take(i) do
      assert(f_.intersection(f) == f_)
  println(s"meet ${System.nanoTime() - meets_t0}")
  // JS  48817795
  // JS  18944244  (warm)
  // JVM 63669039
  // JVM 11935285  (warm)
  // SN   7562551



trait Loader:
  def summarizeResource[T](path: String)(summarize: Iterator[String] => T): T =
    Using(Source.fromFile(path))(f =>
      summarize(f.getLines())
    ).get

object LoadShakespeare extends Loader:
  val TERMINATORS = Array(',', '.', ';', '?', '\"', '-', '[', ']')
  val SEPARATORS = Array(' ', '\t', '\n')
  val IGNORE_CHARS = Set('\'') ++ SEPARATORS ++ TERMINATORS

  def loadSentences(path: String): (RangeStorage[String], ExprMap[Int]) =
    val Sentence = Expr.Var(1)
    val rs = RangeStorage.highPos[String]()
    val em = ExprMap[Int]()

    summarizeResource(path)(_
      .flatMap(_
        .split(TERMINATORS)
        .map(_
          .split(SEPARATORS)
          .map(_.filterNot(IGNORE_CHARS))
          .filterNot(_.isBlank)
          .map(rs.addV)))
      .filterNot(_.isEmpty)
      .map(words => Expr(Sentence, words.head, words.tail*))
      .zipWithIndex
      .foreach((e, ind) => em.update(e, ind))
    )

    (rs, em)


def shakespeare =
  val (rs, em) = LoadShakespeare.loadSentences("/run/media/adamv/Mass-Storage/Datasets/text/t8.shakespeare.txt")

  println(rs.occupied)
  println(em.size)

  val singularity = rs.lookup("singularity").get
  println(singularity)

  val t0 = System.nanoTime()
  println(em.indiscriminateMatching(Expr(Expr.Var(1), Expr.Var(rs.lookup("More").get), $, $, Var(singularity))).prettyListing())
  println(System.nanoTime() - t0)
//

  val t1 = System.nanoTime()
  println(em.indiscriminateMatching(Expr(Expr.Var(1), Expr.Var(rs.lookup("What").get), Expr.Var(rs.lookup("the").get), $)).prettyListing())
  println(System.nanoTime() - t1)


  println((rs.get(33583824), rs.get(33564901), rs.get(33555761)))


  val t2 = System.nanoTime()
  println(em.execute(List(Instr.Tail(1), Instr.Tail(rs.lookup("What").get), Instr.Tail(rs.lookup("the").get))).prettyListing())
  println(System.nanoTime() - t2)


//  val t1 = System.nanoTime()
//  // 33556046
//  println((rs.get(33556082), rs.get(33554778), rs.get(33555187)))
//  println(em.execute(List(Instr.Tail(1), Instr.Tail(33556082), Instr.Tail(33554778), Instr.Tail(33555187), Instr.Tail(singularity))).prettyListing())
//  println(System.nanoTime() - t1)


case class City(
    geonameid         : Int,     // integer id of record in geonames database
    name              : String,  // name of geographical point (utf8) varchar(200)
    asciiname         : String,  // name of geographical point in plain ascii characters, varchar(200)
    alternatenames    : String,  // alternatenames, comma separated, ascii names automatically transliterated, convenience attribute from alternatename table, varchar(10000)
    latitude          : Float,   // latitude in decimal degrees (wgs84)
    longitude         : Float,   // longitude in decimal degrees (wgs84)
    feature_class     : Char,    // see http://www.geonames.org/export/codes.html, char(1)
    feature_code      : String,  // [char; 10], //see http://www.geonames.org/export/codes.html, varchar(10)
    country_code      : String,  // [char; 2], //ISO-3166 2-letter country code, 2 characters
    cc2               : String,  // alternate country codes, comma separated, ISO-3166 2-letter country code, 200 characters
    admin1_code       : String,  // [char; 20], //fipscode (subject to change to iso code), see exceptions below, see file admin1Codes.txt for display names of this code; varchar(20)
    admin2_code       : String,  // code for the second administrative division, a county in the US, see file admin2Codes.txt; varchar(80)
    admin3_code       : String,  // [char; 20], //code for third level administrative division, varchar(20)
    admin4_code       : String,  // [char; 20], //code for fourth level administrative division, varchar(20)
    population        : Long,    // bigint (8 byte int)
    elevation         : Int, // in meters, integer
    dem               : Int, // digital elevation model, srtm3 or gtopo30, average elevation of 3''x3'' (ca 90mx90m) or 30''x30'' (ca 900mx900m) area in meters, integer. srtm processed by cgiar/ciat.
    timezone          : String,  // the iana timezone id (see file timeZone.txt) varchar(40)
    modification_date : String,  // date of last modification in yyyy-MM-dd format
)


object LoadCities extends Loader:
  def fromLine(line: String): City =
    val fs = line.split('\t')
    City(
      geonameid=fs(0).toInt,
      name=fs(1),
      asciiname=fs(2),
      alternatenames=fs(3),
      latitude=fs(4).toFloat,
      longitude=fs(5).toFloat,
      feature_class=fs(6).head,
      feature_code=fs(7),
      country_code=fs(8),
      cc2=fs(9),
      admin1_code=fs(10),
      admin2_code=fs(11),
      admin3_code=fs(12),
      admin4_code=fs(13),
      population=fs(14).toLong,
      elevation=fs(15).toIntOption.getOrElse(0),
      dem=fs(16).toIntOption.getOrElse(0),
      timezone=fs(17),
      modification_date=fs(18),
    )
  end fromLine

  val geoname = Var(1)
  val rscity = RangeStorage(2 << 8, 2 << 29)
  val rsids = rscity.inRange[Int](200_000)
  val rsnames = rscity.inRange[String](200_000)
  val rsasciinames = rscity.inRange[String](2_000_000)
  val rscoords = rscity.inRange[Float](400_000)

  def toExprs(city: City): Seq[Expr] =
    for alternative_name <- city.alternatenames.split(',') yield
      Expr.nest(
        geoname,
        rsids.addV(city.geonameid),
        rsnames.addV(city.name),
        rsasciinames.addV(city.asciiname),
        rsasciinames.addV(alternative_name),
        rscoords.addV(city.latitude),
        rscoords.addV(city.longitude),
      )
  end toExprs

  def loadCities(path: String): ExprMap[(Int, Int)] =
    val em = ExprMap[(Int, Int)]()

    summarizeResource(path)(_
      .map(fromLine)
      .flatMap(city => toExprs(city).zipWithIndex.map((e, i) => (city.geonameid, i) -> e))
      .foreach((id, e) => em.update(e, id))
    )

    em

end LoadCities



def database =
  val t0 = System.nanoTime()
  val em = LoadCities.loadCities("/run/media/adamv/Mass-Storage/Datasets/cities500.txt")
  println(System.nanoTime() - t0)
  println(LoadCities.rsids.occupied -> LoadCities.rsasciinames.occupied)
  println(em.size)

  val tokyo = LoadCities.rsasciinames.lookup("Tokyo").get
  println(s"Tokyo $tokyo")
//  val t1 = System.nanoTime()
//  println(em.indiscriminateMatching(Expr(LoadCities.geoname, $, $, $, Var(tokyo), $, $)).prettyListing())
//  println(System.nanoTime() - t1)

  val t2 = System.nanoTime()
  em.execute(List(Instr.Unapply(1), Instr.Drop, Instr.Drop, Instr.Drop, Unapply(tokyo))).prettyListing()
  println(System.nanoTime() - t2)


def graph =
  val nodes = 1_000
  val edge_frac = 0.03

  val Edge = Expr.Var(1)

  val adj = Array.fill(nodes)(mutable.TreeSet.empty[Int])
  for n <- adj.indices; m <- adj.indices; if Random.nextFloat() < edge_frac do
    adj(n).addOne(m)

  val t0 = System.nanoTime()

  val em_adj = ExprMap[Unit]()
  for (nbs, n) <- adj.zipWithIndex; m <- nbs do
    em_adj.update(Expr(Edge, Expr.Var(n + 10), Expr.Var(m + 10)), ())

  val t1 = System.nanoTime()
  println(s"building took ${t1 - t0} ns")

  val adj_reconstruction = Array.ofDim[ExprMap[Unit]](nodes)
  for n <- adj.indices do
    adj_reconstruction(n) = em_adj.execute(List(Instr.Tail(1), Instr.Unapply(n + 10)))
//    adj_reconstruction(n) = em_adj.transformMatches(Expr(Edge, Expr.Var(n + 10), $), _1)  // equivalent

  val t2 = System.nanoTime()
  println(s"reconstructing took ${t2 - t1} ns")

  assert(adj_reconstruction.zip(adj).forall((l, r) => l.keys.map(_.leftMost - 10).toSet == r.toSet))

//  instruction based construction
//  var em_adj = ExprMap[Unit]()
//  for (nbs, n) <- adj.zipWithIndex do
//    val names = ExprMap.vars(nbs.map(_ + 10L).toArray, Array.fill(nbs.size)(()))
//    em_adj = em_adj.union(names.execute(List(Instr.Apply(n + 10))))
//  em_adj = em_adj.execute(List(Instr.Prepend(1)))


@main
def m1 =
  val Elem = Var(1000)
  val Set1 = Var(2001)
  val Set2 = Var(2002)
  val `1` = Var(1)
  val `2` = Var(2)
  val `3` = Var(3)
  val `10` = Var(10)
  val `20` = Var(20)

  val em = ExprMap[Unit](
    Expr.nest(Elem, Set1, `1`) -> (),
    Expr.nest(Elem, Set1, `2`) -> (),
    Expr.nest(Elem, Set1, `3`) -> (),
//    Expr.nest(Elem, Set2, `10`) -> (),
//    Expr.nest(Elem, Set2, `20`) -> ()
  )

  println(em.prettyStructuredSet())

  println(em.execute(Instr.Unapply(Elem.leftMost)::Instr.Unapply(Set1.leftMost)::Nil).prettyStructuredSet())

  // ⦑⧼1000: ⦑⧼2001: ⧼1, 2, 3⦒⦒⧽⦒⧽
  // ⦑⧼3000: ⦑⧼3001: ⧼1, 2, 3⦒⦒⧽⦒⧽

@main
def m2 =
  val EdgeOf = Var(1000)
  val Graph1 = Var(2001)
  val Graph2 = Var(2002)
  val Graph3 = Var(2003)
  val a = Var(1)
  val b = Var(2)
  val c = Var(3)
  val d = Var(4)
  val e = Var(5)
  val f = Var(6)

  val em = ExprMap[Unit](
    Expr.nest(EdgeOf, Graph1, a, b) -> (),
    Expr.nest(EdgeOf, Graph1, b, c) -> (),
    Expr.nest(EdgeOf, Graph1, b, a) -> (),
    Expr.nest(EdgeOf, Graph1, b, a) -> (),
    Expr.nest(EdgeOf, Graph2, d, e) -> (),
    Expr.nest(EdgeOf, Graph3, f, a) -> (),
    Expr.nest(EdgeOf, Graph3, f, b) -> (),
    Expr.nest(EdgeOf, Graph3, f, c) -> (),
  )

  println(em.prettyStructuredSet())

  println(em.execute(Instr.Unapply(EdgeOf.leftMost)::Instr.Unapply(Graph3.leftMost)::Instr.Unapply(f.leftMost)::Instr.Apply(3001)::Instr.Apply(3000)::Nil).prettyStructuredSet())
