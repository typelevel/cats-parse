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
    val f: Parser[Unit] = parser"foo"
    assertEquals(
      f.parseAll("foo"),
      Right(())
    )
    assertEquals(
      parser"foo".parseAll("foo"),
      Right(())
    )
  }

  test("single parser interpolator") {
    val world: Parser[Int] = Parser.string("world").as(42)
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
    // this is definitely a Parser0
    // this annoying bit is about avoiding a scala 2.11 warning
    // we can't do ${foo} but can do $foo
    assert(compileErrors("""{ val u = Parser.unit; val p: Parser[Unit] = parser"$u" }""").nonEmpty)
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
    // this is definitely a Parser0
    assert(compileErrors("""{ val p: Parser[Unit] = parser"" }""").nonEmpty)
  }

  test("including non-parser args") {
    assert(compileErrors("""{ val i = 42; parser"hello $i" }""").nonEmpty)
  }

  test("a leading or trailing string results in Parser") {
    def n(i: Int) = Parser.string(i.toString).?

    val leading1: Parser[Option[Unit]] = parser"foo${n(1)}"
    assert(leading1.parseAll("foo1").isRight)
    val trailing1: Parser[Option[Unit]] = parser"${n(1)}foo"
    assert(trailing1.parseAll("1foo").isRight)

    val leading2: Parser[(Option[Unit], Option[Unit])] = parser"foo${n(1)}${n(2)}"
    assert(leading2.parseAll("foo12").isRight)
    val trailing2: Parser[(Option[Unit], Option[Unit])] = parser"${n(1)}${n(2)}foo"
    assert(trailing2.parseAll("12foo").isRight)
    val middle2: Parser[(Option[Unit], Option[Unit])] = parser"${n(1)}foo${n(2)}"
    assert(middle2.parseAll("1foo2").isRight)
  }
}
