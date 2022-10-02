package be.adamv

trait EMPrinter:
  import reflect.Typeable
  import compiletime.summonInline

  private val superscriptNumbers = Array("\u2070", "\u00B9", "\u00B2", "\u00B3", "\u2074", "\u2075", "\u2076", "\u2077", "\u2078", "\u2079")
  def superscript(i: Int): String =
    if i >= 10 then superscript(i / 10) + superscriptNumbers(i % 10) else superscriptNumbers(i)

  private val subscriptNumbers = Array("\u2080", "\u2081", "\u2082", "\u2083", "\u2084", "\u2085", "\u2086", "\u2087", "\u2088", "\u2089")
  def subscript(i: Int): String =
    if i >= 10 then subscript(i / 10) + subscriptNumbers(i % 10) else subscriptNumbers(i)

  private val cmap = Array(90, 91, 31, 93, 92, 32, 36, 96, 94, 34, 35, 95, 38)
  def color(t: String, c: Int): String =
    s"\u001b[${cmap(c % cmap.length)}m$t\u001b[0m"

  val newVarString: String
  def preVarString(x: Long): String
  def freeVarString(x: Long): String
  val valSep: String
  val entrySep: String
  val emptyString: String
  val VarBracket: String
  val AppBracket: String
  val VarAppSep: String
  val NoVarBracket: String
  val NoAppBracket: String

  private val ev: Typeable[ExprMap[_]] = summonInline[Typeable[ExprMap[_]]]
  def apply[V](tem: ExprMap[V], depth: Int = 0, colored: Boolean = true): String =
    val em = tem.em
    if em == null then emptyString
    else
      def rec(a: Matchable): String = a match
        case ev(em) => apply(em, depth + 1, colored)
        case o => o.toString

      def process(s: String): String =
        if colored then color(s, depth) else s

      val sections = List(
        em.vars.get(0).map(v => newVarString + valSep + rec(v)),
        em.vars.collect { case (x, v) if x < 0 => preVarString(x) + valSep + rec(v) }.asNonEmpty.map(_.mkString(entrySep)),
        em.vars.collect { case (x, v) if x > 0 => freeVarString(x) + valSep + rec(v) }.asNonEmpty.map(_.mkString(entrySep)),
      ).flatten

      (sections.nonEmpty, em.apps.nonEmpty) match
        case (true, true) => process(VarBracket) + sections.mkString(entrySep) + process(VarAppSep) + apply(em.apps, depth + 1, colored) + process(AppBracket)
        case (true, false) => process(VarBracket) + sections.mkString(entrySep) + process(NoAppBracket)
        case (false, true) => process(NoVarBracket) + apply(em.apps, depth + 1, colored) + process(AppBracket)
        case (false, false) => emptyString

object EMPrettyPrinter extends EMPrinter:
  val newVarString: String = "◆"
  def preVarString(x: Long): String = "⏴" + subscript(-x.toInt)
  def freeVarString(x: Long): String = x.toString
  val valSep: String = ": "
  val entrySep: String = ", "
  val emptyString: String = "∅"
  val VarBracket: String = "⧼"
  val AppBracket: String = "⧽"
  val VarAppSep: String = "|"
  val NoVarBracket: String = "⦑"
  val NoAppBracket: String = "⦒"

object EMJSONPrinter extends EMPrinter:
  val newVarString: String = "\"0\""
  def preVarString(x: Long): String = s"\"$x\""
  def freeVarString(x: Long): String = s"\"$x\""
  val valSep: String = ":"
  val entrySep: String = ","
  val emptyString: String = "null"
  val VarBracket: String = "{"
  val AppBracket: String = "}"
  val VarAppSep: String = ",\"@\":"
  val NoVarBracket: String = "{\"@\":"
  val NoAppBracket: String = "}"

