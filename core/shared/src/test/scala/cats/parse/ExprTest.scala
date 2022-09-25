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
      Operator.InfixN(token(Parser.char('=')).as(Eq.apply))
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
    assertEquals(exprParser.parseAll("1+2*3"), Right(Plus(N("1"), Times(N("2"), N("3")))))
    assert(exprParser.parseAll("1 = 2 = 3").isLeft)
    assertEquals(exprParser.parseAll("1 = 2 + 3"), Right(Eq(N("1"), Plus(N("2"), N("3")))))

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
