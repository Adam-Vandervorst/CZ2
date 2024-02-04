package be.adamv.cz2

import scala.collection.mutable.ArrayBuffer


trait Parser:
  val empty: Int
  val singleton: Int

  def tokenizer(s: String): Expr

  def sexpr(it: Iterator[Char]): Option[Expr] =
    val res = sexprUnsafe(it.buffered)
    Option.unless(res == null)(res)

  def sexprUnsafe(it: collection.BufferedIterator[Char], variables: ArrayBuffer[String] = ArrayBuffer.empty): Expr =
    inline def parseExpr(): Expr =
      var res: Expr = Var(empty)
      it.next()
      while it.head != ')' do
        it.head match
          case c if c.isWhitespace => it.next()
          case _ =>
            res = sexprUnsafe(it, variables)
            if res == null then throw new RuntimeException("Unexpected end of expression member")
            if it.head == ')' then
              res = App(Var(singleton), res)
            else
              while it.head != ')' do
                it.head match
                  case c if c.isWhitespace => it.next()
                  case _ => val continuation = sexprUnsafe(it, variables)
                    if continuation == null then throw new RuntimeException("Unexpected end of expression member")
                    res = App(res, continuation)
      it.next()
      res

    inline def nextToken(): String =
      if it.headOption.contains('"') then nextString()
      else nextIdentifier()

    inline def nextString(): String =
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

    inline def nextIdentifier(): String =
      val sb = StringBuilder()
      var continue = true
      while it.hasNext && continue do
        it.head match
          case '(' | ')' =>
            continue = false
          case c if c.isWhitespace =>
            continue = false
          case c =>
            sb.append(c)
            it.next()
      sb.toString()

    while it.hasNext do
      it.head match
        case ';' =>
          while it.next() != '\n' do ()
        case c if c.isWhitespace =>
          it.next()
        case '$' =>
          val id = nextIdentifier()
          val ind = variables.indexOf(id)
          if ind == -1 then
            variables.append(id)
            return Expr.Var(0)
          else
            return Expr.Var(~ind)
        case '(' =>
          return parseExpr()
        case ')' =>
          throw new RuntimeException("Unexpected right bracket")
        case _ =>
          val e = tokenizer(nextToken())
          if e == null then throw RuntimeException("No backup")
          return e
    null

