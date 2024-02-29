package be.adamv.cz2

import scala.collection.mutable.ArrayBuffer


trait Parser:
  val empty: Int
  val singleton: Int

  def tokenizer(s: String): Option[Expr]

  def sexpr(it: collection.BufferedIterator[Char], variables: ArrayBuffer[String] = ArrayBuffer.empty): Option[Expr] =
    def parseExpr(): Expr =
      val children = ArrayBuffer.empty[Expr]
      it.next()
      while it.head != ')' do
        it.head match
          case c if c.isWhitespace => it.next()
          case _ => children.append(sexpr(it, variables)
            .getOrElse(throw new RuntimeException("Unexpected end of expression member")))
      it.next()
      if children.isEmpty then Var(empty)
      else if children.length == 1 then App(Var(singleton), children.head)
      else Expr(children(0), children(1), children.drop(2).toSeq *)

    def nextToken(): String =
      if it.headOption.contains('"') then nextString()
      else nextIdentifier()

    def nextString(): String =
      val sb = StringBuilder()
      var continue = true
      it.next()
      sb.append('"')
      while it.hasNext && continue do
        it.next() match
          case '"' =>
            sb.append('"')
            continue = false
          case '\\' =>
            if it.hasNext then sb.append(it.next())
            else throw new RuntimeException("Escaping sequence is not finished")
          case c =>
            sb.append(c)
      sb.toString()

    def nextIdentifier(): String =
      it.takeThrough(c => !(c.isWhitespace || c == '(' || c == ')')).mkString

    while it.hasNext do
      it.head match
        case ';' =>
          it.find(_ == '\n')
        case c if c.isWhitespace =>
          it.next()
        case '$' =>
          val id = nextIdentifier()
          val ind = variables.indexOf(id)
          if ind == -1 then
            variables.append(id)
            return Some(Expr.Var(0))
          else
            return Some(Expr.Var(~ind))
        case '(' =>
          return Some(parseExpr())
        case ')' =>
          throw new RuntimeException("Unexpected right bracket")
        case _ =>
          return tokenizer(nextToken()).orElse(throw RuntimeException("No backup"))

    None

