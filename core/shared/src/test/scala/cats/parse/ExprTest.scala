package cats.parse

import cats.parse.expr.ExprParser
import cats.parse.expr.Operator

class ExprTest extends munit.FunSuite {

  sealed trait Exp
  final case class N(n: String) extends Exp
  final case class Neg(a: Exp) extends Exp
  final case class Minus(a: Exp, b: Exp) extends Exp
  final case class Plus(a: Exp, b: Exp) extends Exp
  final case class Times(a: Exp, b: Exp) extends Exp

  test("foo") {

    val term = Numbers.digits.map(N.apply)

    val table: List[List[Operator[Exp]]] = List(
      List(
        Operator.Prefix(Parser.char('-').as(Neg.apply))
      ),
      List(
        Operator.InfixL(Parser.char('*').as(Times.apply))
      ),
      List(
        Operator.InfixL(Parser.char('+').as(Plus.apply)),
        Operator.InfixL(Parser.char('-').as(Minus.apply))
      )
    )

    val exprParser = ExprParser.make(term, table)

    assertEquals(exprParser.parseAll("1"), Right(N("1")))
    assertEquals(exprParser.parseAll("-1"), Right(Neg(N("1"))))
    assertEquals(exprParser.parseAll("1+2"), Right(Plus(N("1"), N("2"))))
    assertEquals(exprParser.parseAll("1+-2"), Right(Plus(N("1"), Neg(N("2")))))
    assertEquals(exprParser.parseAll("1+2-3"), Right(Minus(Plus(N("1"), N("2")), N("3"))))
    assertEquals(exprParser.parseAll("1+2*3"), Right(Plus(N("1"), Times(N("2"), N("3")))))

  }

}
