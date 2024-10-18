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

package cats.parse.bench

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._

import cats.parse.expr.Operator
import cats.parse.expr.ExprParser
import cats.parse.{Parser, Parser0}
import cats.parse.Numbers

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
private[parse] class ExprBenchmarks {

  sealed abstract class Exp
  case class N(n: Int) extends Exp
  case class Neg(a: Exp) extends Exp
  case class Minus(a: Exp, b: Exp) extends Exp
  case class Plus(a: Exp, b: Exp) extends Exp
  case class Times(a: Exp, b: Exp) extends Exp
  case class Div(a: Exp, b: Exp) extends Exp
  case class Eq(a: Exp, b: Exp) extends Exp

  def token[A](p: Parser[A]): Parser[A] =
    p.surroundedBy(Parser.char(' ').rep0)

  val base = token(Numbers.nonNegativeIntString.map(x => N(x.toInt)))

  val table: List[List[Operator[Exp]]] = List(
    List(
      Operator.Prefix(token(Parser.char('-')).as(Neg.apply))
    ),
    List(
      Operator.InfixL(token(Parser.char('*')).as(Times.apply)),
      Operator.InfixL(token(Parser.char('/')).as(Div.apply))
    ),
    List(
      Operator.InfixL(token(Parser.char('+')).as(Plus.apply)),
      Operator.InfixL(token(Parser.char('-')).as(Minus.apply))
    ),
    List(
      Operator.InfixN(token(Parser.string("==")).as(Eq.apply))
    )
  )

  val exprParser = Parser.recursive[Exp] { expr =>
    val term = base | expr.between(
      token(Parser.char('(')),
      token(Parser.char(')'))
    )
    ExprParser.make(term, table)
  }

  /** Manually written parser in the same style as the generated one.
    */
  val manualExprParser = Parser.recursive[Exp] { expr =>
    def factor: Parser[Exp] =
      (token(Parser.char('-')) *> Parser.defer(factor)).map(Neg.apply) | base |
        expr.between(
          token(Parser.char('(')),
          token(Parser.char(')'))
        )

    val plus: Parser[(Exp, Exp) => Exp] = token(Parser.char('+').as(Plus.apply))
    val times: Parser[(Exp, Exp) => Exp] = token(Parser.char('*').as(Times.apply))
    val minus: Parser[(Exp, Exp) => Exp] = token(Parser.char('-').as(Minus.apply))
    val div: Parser[(Exp, Exp) => Exp] = token(Parser.char('/').as(Div.apply))
    val eq: Parser[Unit] = token(Parser.string("=="))

    def term1: Parser0[Exp => Exp] =
      ((times | div) ~ factor ~ Parser.defer0(term1)).map { case ((op, b), f) =>
        (a: Exp) => f(op(a, b))
      } | Parser.pure((x: Exp) => x)

    def term: Parser[Exp] = (factor ~ term1).map { case (a, f) => f(a) }

    def arith1: Parser0[Exp => Exp] =
      ((plus | minus) ~ term ~ Parser.defer0(arith1)).map { case ((op, b), f) =>
        (a: Exp) => f(op(a, b))
      } | Parser.pure((x: Exp) => x)

    def arith: Parser[Exp] = (term ~ arith1).map { case (a, f) => f(a) }

    arith ~ (eq *> arith).? map {
      case (e1, None) => e1
      case (e1, Some(e2)) => Eq(e1, e2)
    }

  }

  @Benchmark
  def manual: Exp =
    manualExprParser.parseAll("5 * ((1 + 2) * 3 / -4) == 5 + 2") match {
      case Right(e) => e
      case Left(e) => sys.error(e.toString)
    }

  def tabular: Exp =
    exprParser.parseAll("5 * ((1 + 2) * 3 / -4) == 5 + 2") match {
      case Right(e) => e
      case Left(e) => sys.error(e.toString)
    }

}
