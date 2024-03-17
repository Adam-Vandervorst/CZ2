package be.adamv.cz2

trait ExprMapPrinter extends Printer:
  import reflect.Typeable
  import compiletime.summonInline

  val valSep: String
  val entrySep: String
  val emptyString: String
  val VarBracket: String
  val AppBracket: String
  val AppVarSep: String
  val NoVarBracket: String
  val NoAppBracket: String
  val indentWhitespace: String = "  "

  private val ev: Typeable[ExprMap[_]] = summonInline[Typeable[ExprMap[_]]]
  def structured[V](tem: ExprMap[V], depth: Int = 0, colored: Boolean = true, tree: Boolean = false): String =
    val em = tem.em
    if em == null then emptyString
    else
      def rec(a: Matchable): String = a match
        case ev(em) => structured(em, depth + 1, colored)
        case o => o.toString

      def process(s: String): String =
        if colored then color(s, depth) else s

      val sections = List(
        em.vars.get(0).map(v => newVarString + valSep + rec(v)),
        em.vars.iterator.collect { case (x, v) if x < 0 => preVarString(x) + valSep + rec(v) }.toVector.asNonEmpty.map(_.mkString(entrySep)),
        em.vars.iterator.collect { case (x, v) if x > 0 => freeVarString(x) + valSep + rec(v) }.toVector.asNonEmpty.map(_.mkString(entrySep)),
      ).flatten

      (sections.nonEmpty, em.apps.nonEmpty) match
        case (true, true) => process(AppBracket) + structured(em.apps, depth + 1, colored) + process(AppVarSep) + sections.mkString(entrySep) + process(VarBracket)
        case (true, false) => process(NoAppBracket) + sections.mkString(entrySep) + process(VarBracket)
        case (false, true) => process(AppBracket) + structured(em.apps, depth + 1, colored) + process(NoVarBracket)
        case (false, false) => emptyString

  def structuredSet(tem: ExprMap[_], depth: Int = 0, acc: Int = 0, colored: Boolean = true, tree: Boolean = false): String =
    val em = tem.em
    if em == null then emptyString
    else
      def rec(prefix: String, a: Matchable): String = a match
          case ev(em) => prefix + valSep + structuredSet(em, depth + 1, acc + prefix.length + valSep.length, colored, tree)
          case _ => prefix

      def process(s: String): String =
        if colored then color(s, depth) else s

      val sections = List(
        em.vars.get(0).map(v => rec(newVarString, v)),
        em.vars.iterator.collect { case (x, v) if x < 0 => rec(preVarString(x), v) },
        em.vars.iterator.collect { case (x, v) if x > 0 => rec(freeVarString(x), v) },
      ).flatten.mkString(if tree then entrySep + "\n" + " ".repeat(acc + 1) else entrySep)

      (em.vars.nonEmpty, em.apps.nonEmpty) match
        case (true, true) => process(AppBracket) + structuredSet(em.apps, depth + 1, acc + AppBracket.length, colored, tree) + (
          if tree then "\n" + " ".repeat(acc).drop(AppVarSep.length) + process(AppVarSep) else process(AppVarSep)
          ) + sections + process(VarBracket)
        case (true, false) => process(NoAppBracket) + sections + process(VarBracket)
        case (false, true) => process(AppBracket) + structuredSet(em.apps, depth + 1, acc + AppBracket.length, colored, tree) + process(NoVarBracket)
        case (false, false) => emptyString

  def listing(tem: ExprMap[_], colored: Boolean = true): String =
    val r = tem.keys.map(sexpression(_, 0, colored)).mkString(entrySep)
    if r == "" then emptyString
    else r


object EMPrettyPrinter extends ExprMapPrinter:
  val newVarString: String = "◆"
  def preVarString(x: Long): String = "⏴" + subscript(-x.toInt)
  def freeVarString(x: Long): String = x.toString
  val exprOpen: String = "("
  val exprClose: String = ")"
  val exprSep: String = " "
  val valSep: String = ": "
  val entrySep: String = ", "
  val emptyString: String = "∅"
  val VarBracket: String = "⧽"
  val AppBracket: String = "⧼"
  val AppVarSep: String = "|"
  val NoVarBracket: String = "⦒"
  val NoAppBracket: String = "⦑"

object EMJSONPrinter extends ExprMapPrinter:
  val newVarString: String = "\"0\""
  def preVarString(x: Long): String = s"\"$x\""
  def freeVarString(x: Long): String = s"\"$x\""
  val exprOpen: String = "["
  val exprClose: String = "]"
  val exprSep: String = ","
  val valSep: String = ":"
  val entrySep: String = ","
  val emptyString: String = "null"
  val VarBracket: String = "}"
  val AppBracket: String = "{"
  val AppVarSep: String = ",\"@\":"
  val NoVarBracket: String = "}"
  val NoAppBracket: String = "{\"@\":"

object EMListPrinter extends ExprMapPrinter:
  val newVarString: String = "◆"
  def preVarString(x: Long): String = "⏴" + subscript(-x.toInt)
  def freeVarString(x: Long): String = x.toString
  val exprOpen: String = "("
  val exprClose: String = ")"
  val exprSep: String = " "
  val valSep: String = " -> "
  val entrySep: String = "\n"
  val emptyString: String = "<empty>"
  val VarBracket: String = "???"
  val AppBracket: String = "???"
  val AppVarSep: String = "???"
  val NoVarBracket: String = "???"
  val NoAppBracket: String = "???"
