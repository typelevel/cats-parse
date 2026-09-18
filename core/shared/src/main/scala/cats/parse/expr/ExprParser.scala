/*
 * Copyright (c) 2021 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats.parse.expr

import cats.parse.{Parser, Parser0}

object ExprParser {

  /** Parser of binary operator
    */
  type BinP[A] = Parser[(A, A) => A]

  /** Parser of unary operator
    */
  type UnP[A] = Parser[A => A]

  /** Takes a parser for terms and a list of operator precedence levels and returns a parser of
    * expressions.
    */
  def make[A](term: Parser[A], table: List[List[Operator[A]]]): Parser[A] =
    table.foldLeft(term)(addPrecLevel)

  /** Internal helper class for splitting an operator precedence level into the various types.
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

    def parseInfixN(op: BinP[A], term: Parser[A]): Parser[A => A] =
      (op ~ term).map { case (op, b) => a => op(a, b) }

    def parseInfixL(op: BinP[A], term: Parser[A]): Parser[A => A] =
      (op ~ term ~ Parser.defer(parseInfixL(op, term)).?)
        .map {
          case ((op, b), None) => (a: A) => op(a, b)
          case ((op, b), Some(rest)) => (a: A) => rest(op(a, b))
        }

    def parseInfixR(op: BinP[A], term: Parser[A]): Parser[A => A] =
      (op ~ term ~ Parser.defer(parseInfixR(op, term)).?)
        .map {
          case ((op, b), None) => (a: A) => op(a, b)
          case ((op, b), Some(rest)) => (a: A) => op(a, rest(b))
        }

    /** Try to parse a term prefixed or postfixed
      */
    val term_ = parseTerm(Parser.oneOf(batch.pre), Parser.oneOf(batch.post))

    /** Then try the operators in the precedence level
      */
    (term_ ~
      Parser
        .oneOf0(
          List(
            parseInfixR(Parser.oneOf(batch.inr), term_),
            parseInfixN(Parser.oneOf(batch.inn), term_),
            parseInfixL(Parser.oneOf(batch.inl), term_)
          )
        )
        .?).map {
      case (x, None) => x
      case (x, Some(rest)) => rest(x)
    }

  }
}
