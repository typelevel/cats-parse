package cats.parse.expr

import cats.parse.Parser

/** This an entry in the operator table used to build an expression parser
  */
sealed abstract class Operator[A]
object Operator {

  /** Non-associative infix operator. Example: &&, ||
   */
  final case class InfixN[A](c: Parser[(A, A) => A]) extends Operator[A]

  /** Left-associative infix operator. Example: +, *, /
   */
  final case class InfixL[A](c: Parser[(A, A) => A]) extends Operator[A]

  /** Right-associative infix operator. Example: assignment in C
   */
  final case class InfixR[A](c: Parser[(A, A) => A]) extends Operator[A]

  /** Prefix operator like for instance unary negation
   */
  final case class Prefix[A](c: Parser[A => A]) extends Operator[A]

  /** Postfix operator
   */
  final case class Postfix[A](c: Parser[A => A]) extends Operator[A]
}
