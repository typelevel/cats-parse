package cats.parse.expr

import cats.parse.Parser

sealed abstract class Operator[A]
object Operator {

  final case class InfixN[A](c: Parser[(A, A) => A]) extends Operator[A]
  final case class InfixL[A](c: Parser[(A, A) => A]) extends Operator[A]
  final case class InfixR[A](c: Parser[(A, A) => A]) extends Operator[A]
  final case class Prefix[A](c: Parser[A => A]) extends Operator[A]
  final case class Postfix[A](c: Parser[A => A]) extends Operator[A]
}
