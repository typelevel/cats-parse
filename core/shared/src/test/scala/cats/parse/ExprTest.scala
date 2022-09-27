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

package cats.parse

import cats.parse.expr.ExprParser
import cats.parse.expr.Operator

class ExprTest extends munit.FunSuite {

  sealed trait Exp
  final case class N(n: String) extends Exp
  final case class Neg(a: Exp) extends Exp
  final case class Q(a: Exp) extends Exp
  final case class Minus(a: Exp, b: Exp) extends Exp
  final case class Plus(a: Exp, b: Exp) extends Exp
  final case class Times(a: Exp, b: Exp) extends Exp
  final case class And(a: Exp, b: Exp) extends Exp
  final case class Eq(a: Exp, b: Exp) extends Exp

  def token[A](p: Parser[A]): Parser[A] =
    p.surroundedBy(Rfc5234.sp.rep0)

  val table: List[List[Operator[Exp]]] = List(
    List(
      Operator.Prefix(token(Parser.char('-')).as(Neg.apply)),
      Operator.Postfix(token(Parser.char('?')).as(Q.apply))
    ),
    List(
      Operator.InfixL(token(Parser.char('*')).as(Times.apply))
    ),
    List(
      Operator.InfixL(token(Parser.char('+')).as(Plus.apply)),
      Operator.InfixL(token(Parser.char('-')).as(Minus.apply))
    ),
    List(
      Operator.InfixN(token(Parser.char('&')).as(And.apply)),
      Operator.InfixR(token(Parser.char('=')).as(Eq.apply))
    )
  )

  test("non-recursive") {

    val term = token(Numbers.digits.map(N.apply))
    val exprParser = ExprParser.make(term, table)

    assertEquals(exprParser.parseAll("1"), Right(N("1")))
    assertEquals(exprParser.parseAll("-1"), Right(Neg(N("1"))))
    assertEquals(exprParser.parseAll("-1?"), Right(Q(Neg(N("1")))))
    assertEquals(exprParser.parseAll("1 + 2"), Right(Plus(N("1"), N("2"))))
    assertEquals(exprParser.parseAll("1 + -2"), Right(Plus(N("1"), Neg(N("2")))))
    assertEquals(exprParser.parseAll("1 + 2 - 3"), Right(Minus(Plus(N("1"), N("2")), N("3"))))
    assertEquals(exprParser.parseAll("1 + 2 + 3 + 4"), Right(Plus(Plus(Plus(N("1"), N("2")), N("3")), N("4"))))
    assertEquals(exprParser.parseAll("1+2*3"), Right(Plus(N("1"), Times(N("2"), N("3")))))
    assert(exprParser.parseAll("1 & 2 & 3").isLeft)
    assertEquals(exprParser.parseAll("1 = 2"), Right(Eq(N("1"), N("2"))))
    assertEquals(exprParser.parseAll("1 & 2"), Right(And(N("1"), N("2"))))
    assertEquals(exprParser.parseAll("1 = 2 + 3"), Right(Eq(N("1"), Plus(N("2"), N("3")))))
    assertEquals(exprParser.parseAll("1 = 2 = 3"), Right(Eq(N("1"), Eq(N("2"), N("3")))))

  }

  test("recursive") {

    val exprParser = Parser.recursive[Exp] { expr =>
      val term = token(Numbers.digits.map(N.apply)) | expr.between(
        token(Parser.char('(')),
        token(Parser.char(')'))
      )
      ExprParser.make(term, table)
    }

    assertEquals(exprParser.parseAll("( (2) )"), Right(N("2")))
    assertEquals(exprParser.parseAll("-(-1)"), Right(Neg(Neg(N("1")))))
    assertEquals(exprParser.parseAll("1*( 2+3 )"), Right(Times(N("1"), Plus(N("2"), N("3")))))
    assertEquals(exprParser.parseAll("(1 = 2) = 3?"), Right(Eq(Eq(N("1"), N("2")), Q(N("3")))))

  }

}
