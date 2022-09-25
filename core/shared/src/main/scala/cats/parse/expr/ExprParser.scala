package cats.parse.expr

import cats.parse.{Parser, Parser0}

object ExprParser {

  type BinP[A] = Parser[(A, A) => A]
  type UnP[A] = Parser[A => A]

  /** Takes a parser for terms and a list of operator precedence levels and returns a parser of
    * expressions.
    */
  def make[A](term: Parser[A], table: List[List[Operator[A]]]): Parser[A] =
    table.foldLeft(term)(addPrecLevel)

  /** Internal helper class for splitting an operator precedence level into the varios types.
    */
  private final case class Batch[A](
      inn: List[BinP[A]],
      inl: List[BinP[A]],
      inr: List[BinP[A]],
      pre: List[UnP[A]],
      post: List[UnP[A]]
  ) {
    def add(op: Operator[A]): Batch[A] = op match {
      case Operator.InfixN(p) => this.copy(inn = p :: inn)
      case Operator.InfixL(p) => this.copy(inl = p :: inl)
      case Operator.InfixR(p) => this.copy(inr = p :: inr)
      case Operator.Prefix(p) => this.copy(pre = p :: pre)
      case Operator.Postfix(p) => this.copy(post = p :: post)
    }
  }
  private object Batch {
    def empty[A]: Batch[A] =
      Batch(List.empty, List.empty, List.empty, List.empty, List.empty)

    def apply[A](level: List[Operator[A]]): Batch[A] =
      level.foldRight(Batch.empty[A]) { (op, b) => b.add(op) }
  }

  private def addPrecLevel[A](p: Parser[A], level: List[Operator[A]]): Parser[A] = {

    val batch = Batch(level)

    def orId(p: Parser[A => A]): Parser0[A => A] =
      p.orElse(Parser.pure((x: A) => x))

    def parseTerm(prefix: UnP[A], postfix: UnP[A]): Parser[A] =
      (orId(prefix).with1 ~ p ~ orId(postfix)).map { case ((pre, t), post) =>
        post(pre(t))
      }

    def parseInfixN(op: BinP[A], term: Parser[A], a: A): Parser[A] =
      (op ~ term).map { case (f, y) => f(a, y) }

    def parseInfixL(op: BinP[A], term: Parser[A], a: A): Parser[A] =
      (op ~ term).flatMap { case (f, y) =>
        parseInfixL(op, term, f(a, y)) | Parser.pure(f(a, y))
      }

    def parseInfixR(op: BinP[A], term: Parser[A], a: A): Parser[A] =
      (op ~ (term.flatMap(r => parseInfixR(op, term, a) | Parser.pure(r)))).map { case (f, y) =>
        f(a, y)
      }

    /** Try to parse a term prefixed or postfixed
      */
    val term_ = parseTerm(Parser.oneOf(batch.pre), Parser.oneOf(batch.post))

    /** Then try the operators in the precedence level
      */
    term_.flatMap(x =>
      Parser.oneOf0(
        List(
          parseInfixR(Parser.oneOf(batch.inr), term_, x),
          parseInfixN(Parser.oneOf(batch.inn), term_, x),
          parseInfixL(Parser.oneOf(batch.inl), term_, x),
          Parser.pure(x)
        )
      )
    )

  }
}
