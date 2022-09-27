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
