package be.adamv.cz2

import scala.annotation.tailrec

trait Printer:
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
  val exprSep: String
  val exprOpen: String
  val exprClose: String

  def sexpression(e: Expr, depth: Int = 0, colored: Boolean = true): String =
    def process(s: String): String =
      if colored then color(s, depth) else s

    (e: @unchecked) match
      case Var(0) => newVarString
      case Var(i) if i < 0 => preVarString(i)
      case Var(i) => freeVarString(i)
      case Expr(es: _*) =>
        process(exprOpen) + es.map(sexpression(_, depth + 1, colored)).mkString(exprSep) + process(exprClose)


class NamedPrettyPrinter(names: RangeStorage[String]) extends Printer:
  val newVarString: String = "◆"
  def preVarString(x: Long): String = freeVarString(x)
  def freeVarString(x: Long): String = names.get(x.toInt).getOrElse(x.toString)
  val exprSep: String = " "
  val exprOpen: String = "("
  val exprClose: String = ")"

object PrettyPrinter extends Printer:
  val newVarString: String = "◆"
  def preVarString(x: Long): String = "⏴" + subscript(-x.toInt)
  def freeVarString(x: Long): String = x.toString
  val exprSep: String = " "
  val exprOpen: String = "("
  val exprClose: String = ")"

object ShowPrinter extends Printer:
  val newVarString: String = "Var(0)"
  def preVarString(x: Long): String = s"Var($x)"
  def freeVarString(x: Long): String = s"Var($x)"
  val exprSep: String = ", "
  val exprOpen: String = "Expr("
  val exprClose: String = ")"

object JSONPrinter extends Printer:
  val newVarString: String = "0"
  def preVarString(x: Long): String = x.toString
  def freeVarString(x: Long): String = x.toString
  val exprSep: String = ","
  val exprOpen: String = "["
  val exprClose: String = "]"
