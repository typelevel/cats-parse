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
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen

object ExprTest {

  sealed abstract class Exp
  final case class N(n: Int) extends Exp
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
      Operator.InfixN(token(Parser.char('&')).as(And.apply))
    ),
    List(
      Operator.InfixR(token(Parser.char('=')).as(Eq.apply))
    )
  )

  def pLevel(e: Exp): Int = e match {
    case N(_) => -1
    case Neg(_) | Q(_) => 0
    case Times(_, _) => 1
    case Plus(_, _) | Minus(_, _) => 2
    case And(_, _) => 3
    case Eq(_, _) => 4
  }

  /** Quick and dirty pretty printer than inserts few parenthesis
    */
  def pp(e: Exp): String = {

    // Left-associative. Precedence level should be lower in right branch
    def leftAssoc(level: Int, a: Exp, b: Exp, op: String): String = {
      val leftStr = if (pLevel(a) <= level) pp(a) else s"(${pp(a)})"
      val rightStr = if (pLevel(b) < level) pp(b) else s"(${pp(b)})"
      s"$leftStr $op $rightStr"
    }
    e match {
      case N(n) => n.toString

      case Neg(a) =>
        if (pLevel(a) < 0) s"-${pp(a)}" else s"-(${pp(a)})"

      case Q(a) =>
        if (pLevel(a) < 0) s"${pp(a)}?" else s"(${pp(a)})?"

      case Times(a, b) => leftAssoc(1, a, b, "*")
      case Plus(a, b) => leftAssoc(2, a, b, "+")
      case Minus(a, b) => leftAssoc(2, a, b, "-")

      // Non-associative. Precedence level should be lower in both branches
      case And(a, b) => {
        val leftStr = if (pLevel(a) < 3) pp(a) else s"(${pp(a)})"
        val rightStr = if (pLevel(b) < 3) pp(b) else s"(${pp(b)})"
        s"$leftStr & $rightStr"
      }

      // Right-associative. Precedence level should be lower in left branch
      case Eq(a, b) => {
        val leftStr = if (pLevel(a) < 4) pp(a) else s"(${pp(a)})"
        val rightStr = if (pLevel(b) <= 4) pp(b) else s"(${pp(b)})"
        s"$leftStr = $rightStr"
      }
    }
  }

  val genN: Gen[Exp] = Gen.choose(0, 100).map(N.apply)

  val genAnd: Gen[Exp] = genExp.flatMap(a => genExp.map(b => And(a, b)))
  val genEq: Gen[Exp] = genExp.flatMap(a => genExp.map(b => Eq(a, b)))
  val genPlus: Gen[Exp] = genExp.flatMap(a => genExp.map(b => Plus(a, b)))
  val genMinus: Gen[Exp] = genExp.flatMap(a => genExp.map(b => Minus(a, b)))
  val genTimes: Gen[Exp] = genExp.flatMap(a => genExp.map(b => Times(a, b)))
  val genNeg: Gen[Exp] = genExp.map(a => Neg(a))
  val genQ: Gen[Exp] = genExp.map(a => Q(a))

  def genExp: Gen[Exp] = Gen.sized(n =>
    if (n <= 1) genN
    else
      Gen.oneOf(
        Gen.lzy(Gen.resize(n / 2, genAnd)),
        Gen.lzy(Gen.resize(n / 2, genEq)),
        Gen.lzy(Gen.resize(n / 2, genPlus)),
        Gen.lzy(Gen.resize(n / 2, genMinus)),
        Gen.lzy(Gen.resize(n / 2, genTimes)),
        Gen.lzy(Gen.resize(n - 1, genNeg)),
        Gen.lzy(Gen.resize(n - 1, genQ))
      )
  )

}

class ExprTest extends munit.ScalaCheckSuite {

  import ExprTest._

  test("non-recursive") {

    val term = token(Numbers.nonNegativeIntString.map(x => N(x.toInt)))
    val exprParser = ExprParser.make(term, table)

    assertEquals(exprParser.parseAll("1"), Right(N(1)))
    assertEquals(exprParser.parseAll("-1"), Right(Neg(N(1))))
    assertEquals(exprParser.parseAll("-1?"), Right(Q(Neg(N(1)))))
    assertEquals(exprParser.parseAll("1 + 2"), Right(Plus(N(1), N(2))))
    assertEquals(exprParser.parseAll("1 + -2"), Right(Plus(N(1), Neg(N(2)))))
    assertEquals(exprParser.parseAll("1 + 2 - 3"), Right(Minus(Plus(N(1), N(2)), N(3))))
    assertEquals(
      exprParser.parseAll("1 + 2 + 3 + 4"),
      Right(Plus(Plus(Plus(N(1), N(2)), N(3)), N(4)))
    )
    assertEquals(exprParser.parseAll("1+2*3"), Right(Plus(N(1), Times(N(2), N(3)))))
    assert(exprParser.parseAll("1 & 2 & 3").isLeft)
    assertEquals(exprParser.parseAll("1 = 2"), Right(Eq(N(1), N(2))))
    assertEquals(exprParser.parseAll("1 & 2"), Right(And(N(1), N(2))))
    assertEquals(exprParser.parseAll("1 = 2 + 3"), Right(Eq(N(1), Plus(N(2), N(3)))))
    assertEquals(exprParser.parseAll("1 = 2 = 3"), Right(Eq(N(1), Eq(N(2), N(3)))))
    assertEquals(exprParser.parseAll("1 = 2 & 3"), Right(Eq(N(1), And(N(2), N(3)))))
    assertEquals(exprParser.parseAll("1 & 2 = 3"), Right(Eq(And(N(1), N(2)), N(3))))

  }

  test("recursive") {

    val exprParser = Parser.recursive[Exp] { expr =>
      val term = token(Numbers.nonNegativeIntString.map(n => N(n.toInt))) | expr.between(
        token(Parser.char('(')),
        token(Parser.char(')'))
      )
      ExprParser.make(term, table)
    }

    assertEquals(exprParser.parseAll("( (2) )"), Right(N(2)))
    assertEquals(exprParser.parseAll("-(-1)"), Right(Neg(Neg(N(1)))))
    assertEquals(exprParser.parseAll("1*( 2+3 )"), Right(Times(N(1), Plus(N(2), N(3)))))
    assertEquals(exprParser.parseAll("(1 = 2) = 3?"), Right(Eq(Eq(N(1), N(2)), Q(N(3)))))

  }

  property("foo") {

    val exprParser = Parser.recursive[Exp] { expr =>
      val term = token(Numbers.nonNegativeIntString.map(x => N(x.toInt))) | expr.between(
        token(Parser.char('(')),
        token(Parser.char(')'))
      )
      ExprParser.make(term, table)
    }

    forAll(genExp) { (e: Exp) =>
      assertEquals(exprParser.parseAll(pp(e)), Right(e))
    }

  }

}
