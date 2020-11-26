/*
 * Copyright (c) 2020 Typelevel
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

/* Based on https://github.com/tpolecat/atto/blob/v0.8.0/modules/docs/src/main/scala/json.scala */
package cats.parse.bench.atto

import _root_.atto._, Atto._
import cats.implicits._
import java.lang.String
import org.typelevel.jawn.ast._

object JsonExample extends Whitespace {
  // Bracketed, comma-separated sequence, internal whitespace allowed
  def seq[A](open: Char, p: Parser[A], close: Char): Parser[List[A]] =
    char(open).t ~> sepByT(p, char(',')) <~ char(close)

  // Colon-separated pair, internal whitespace allowed
  lazy val pair: Parser[(String, JValue)] =
    pairByT(stringLiteral, char(':'), jexpr)

  // Json Expression
  lazy val jexpr: Parser[JValue] = delay {
    stringLiteral -| JString.apply |
      seq('{', pair, '}') -| JObject.fromSeq |
      seq('[', jexpr, ']') -| JArray.fromSeq |
      double -| JNum.apply |
      string("null") >| JNull |
      string("true") >| JTrue |
      string("false") >| JFalse
  }

}

// Some extre combinators and syntax for coping with whitespace. Something like this might be
// useful in core but it needs some thought.
trait Whitespace {

  // Syntax for turning a parser into one that consumes trailing whitespace
  implicit class TokenOps[A](self: Parser[A]) {
    def t: Parser[A] =
      self <~ takeWhile(c => c.isSpaceChar || c === '\n')
  }

  // Delimited list
  def sepByT[A](a: Parser[A], b: Parser[_]): Parser[List[A]] =
    sepBy(a.t, b.t)

  // Delimited pair, internal whitespace allowed
  def pairByT[A, B](a: Parser[A], delim: Parser[_], b: Parser[B]): Parser[(A, B)] =
    pairBy(a.t, delim.t, b)

}
