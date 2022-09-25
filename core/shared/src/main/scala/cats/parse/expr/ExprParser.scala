package cats.parse.expr

import cats.parse.{Parser, Parser0}

object ExprParser {

  import Operator.{BinP, UnP}

  def make[A](term: Parser[A], table: List[List[Operator[A]]]): Parser[A] =
    table.foldLeft(term)(addPrecLevel)

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
  }

  private def addPrecLevel[A](p: Parser[A], level: List[Operator[A]]): Parser[A] = {

    val batch: Batch[A] = level.foldRight(Batch.empty[A]) { (op, b) => b.add(op) }

    def orId(p: Parser[A => A]): Parser0[A => A] =
      p.orElse(Parser.pure((x: A) => x))

    def pTerm(prefix: UnP[A], postfix: UnP[A]): Parser[A] =
      (orId(prefix).with1 ~ p ~ orId(postfix)).map { case ((pre, t), post) =>
        post(pre(t))
      }

    def pInfixN(op: BinP[A], term: Parser[A], a: A): Parser[A] =
      (op ~ term).map { case (f, y) => f(a, y) }

    def pInfixL(op: BinP[A], term: Parser[A], a: A): Parser[A] =
      (op ~ term).flatMap { case (f, y) =>
        pInfixL(op, term, f(a, y)) | Parser.pure(f(a, y))
      }

    def pInfixR(op: BinP[A], term: Parser[A], a: A): Parser[A] =
      (op ~ (term.flatMap(r => pInfixR(op, term, a) | Parser.pure(r)))).map { case (f, y) =>
        f(a, y)
      }

    val term_ = pTerm(Parser.oneOf(batch.pre), Parser.oneOf(batch.post))

    term_.flatMap(x =>
      Parser.oneOf0(
        List(
          pInfixR(Parser.oneOf(batch.inr), term_, x),
          pInfixN(Parser.oneOf(batch.inn), term_, x),
          pInfixL(Parser.oneOf(batch.inl), term_, x),
          Parser.pure(x)
        )
      )
    )

  }
}
