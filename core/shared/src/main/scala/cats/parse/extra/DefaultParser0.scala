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

import cats.parse.{Parser, Parser0}

/**
 * Typeclass for "has a Parser0"
 *
 * This is primarily provided to help keep track of `Parser0` instances, and as such the omission of
 * `cats.Functor` instances is intentional.
 * @tparam A
 */
trait DefaultParser0[+A] {
  def parser0: Parser0[A]
}
object DefaultParser0 {
  def apply[A](implicit P: DefaultParser0[A]): P.type = P

  def instance[A](p: Parser0[A]): DefaultParser0[A] = new Impl[A](p)

  private final class Impl[+A](override val parser0: Parser0[A])
    extends DefaultParser0[A]
      with Serializable

  object syntax {
    implicit final class DefaultParser0Ops(private val raw: String) extends AnyVal {
      def parse[A: DefaultParser0]: Either[Parser.Error, (String, A)] =
        DefaultParser0[A].parser0.parse(raw)

      def parseAll[A: DefaultParser0]: Either[Parser.Error, A] =
        DefaultParser0[A].parser0.parseAll(raw)
    }
  }

  implicit def defaultParserIsDefaultParser0[A: DefaultParser]: DefaultParser0[A] =
    DefaultParser0.instance(DefaultParser[A].parser)
}