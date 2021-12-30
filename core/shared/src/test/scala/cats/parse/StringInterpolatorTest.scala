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

import StringInterpolation._

class StringInterpolatorTest extends munit.ScalaCheckSuite {

  test("parser string interpolator") {
    val foo: Parser[Boolean] = Parser.string("spam").as(true) | Parser.string("eggs").as(false)
    val bar: Parser[BigInt] = Numbers.bigInt
    val quux: Parser[Int] = Parser.string("quux").as(42)

    val arrow: Parser[(Boolean, BigInt, Int)] = parser"$foo,$bar -> $quux"

    assertEquals(
      arrow.parseAll("spam,1234 -> quux"),
      Right((true, BigInt(1234), 42))
    )
  }
}
