package cats.parse.expr

import cats.parse.Parser

sealed abstract class Operator[A]
object Operator {

  type BinP[A] = Parser[(A, A) => A]
  type UnP[A] = Parser[A => A]

  final case class InfixN[A](c: BinP[A]) extends Operator[A]
  final case class InfixL[A](c: BinP[A]) extends Operator[A]
  final case class InfixR[A](c: BinP[A]) extends Operator[A]
  final case class Prefix[A](c: UnP[A]) extends Operator[A]
  final case class Postfix[A](c: UnP[A]) extends Operator[A]
}
