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

  test("zero parser interpolation") {
    assertEquals(
      parser"foo".parseAll("foo"),
      Right(())
    )
  }

  test("single parser interpolator") {
    val world = Parser.string("world").as(42)
    val parser: Parser[Int] = parser"hello $world"

    assertEquals(
      parser.parseAll("hello world"),
      Right(42)
    )
  }

  test("trailing string literal") {
    val hello = Parser.string("hello").as(42)
    val parser: Parser[Int] = parser"$hello world"

    assertEquals(
      parser.parseAll("hello world"),
      Right(42)
    )
  }

  test("multi-parser interpolation") {
    val foo: Parser[Boolean] = Parser.string("spam").as(true) | Parser.string("eggs").as(false)
    val bar: Parser[BigInt] = Numbers.bigInt
    val quux: Parser[Int] = parser"quux".as(42)

    val arrow: Parser[(Boolean, BigInt, Int)] = parser"$foo,$bar -> $quux"

    assertEquals(
      arrow.parseAll("spam,1234 -> quux"),
      Right((true, BigInt(1234), 42))
    )

  }

  test("including a Parser0") {
    assertEquals(
      parser"foo${Parser.unit}".parseAll("foo"),
      Right(())
    )
  }

  test("single Parser0") {
    assertEquals(
      parser"${Parser.unit}".parseAll(""),
      Right(())
    )

  }

  test("multiple Parser0") {
    assertEquals(
      parser"${Parser.unit}${Parser.unit}".parseAll(""),
      Right(((), ()))
    )
  }

  test("empty string") {
    assertEquals(
      parser"".parseAll(""),
      Right(())
    )
  }

  test("including non-parser args") {
    assert(compileErrors("""{ val i = 42; parser"hello $i" }""").nonEmpty)
  }

  test("Parser vs Parser0") {
    val p: Parser[Unit] = Parser.string("foo")
    val p0: Parser0[Unit] = Parser.unit

    assertEquals((parser"": Parser0[Unit]).parseAll(""), Right(()))
    assertEquals((parser"$p": Parser[Unit]).parseAll("foo"), Right(()))
    assertEquals((parser"$p0": Parser0[Unit]).parseAll(""), Right(()))
    assertEquals((parser"foo$p0": Parser[Unit]).parseAll("foo"), Right(()))
    assertEquals((parser"$p0$p0": Parser0[(Unit, Unit)]).parseAll(""), Right(((), ())))
    assertEquals((parser"$p0$p": Parser[(Unit, Unit)]).parseAll("foo"), Right(((), ())))
  }

}
