/*
 * Copyright (c) 2024 Typelevel
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

package cats.parse.extra

import cats.parse.Parser

/**
 * Typeclass for "has a Parser"
 *
 * This is primarily provided to help keep track of `Parser` instances, and as such the omission of
 * `cats.Functor` instances is intentional.
 * @tparam A
 */
trait DefaultParser[+A] {
  def parser: Parser[A]
}
object DefaultParser {
  def apply[A](implicit P: DefaultParser[A]): P.type = P

  def instance[A](p: Parser[A]): DefaultParser[A] = new Impl[A](p)

  private final class Impl[+A](override val parser: Parser[A])
    extends DefaultParser[A]
      with Serializable

  object syntax {
    implicit final class DefaultParserOps(private val raw: String) extends AnyVal {
      def parse[A: DefaultParser]: Either[Parser.Error, (String, A)] =
        DefaultParser[A].parser.parse(raw)

      def parseAll[A: DefaultParser]: Either[Parser.Error, A] =
        DefaultParser[A].parser.parseAll(raw)
    }
  }
}