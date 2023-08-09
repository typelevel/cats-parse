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

import cats.implicits.toShow

import org.scalacheck.Prop.forAll

import Parser._
import Numbers.digits

class ErrorShowTest extends munit.ScalaCheckSuite {

  def error(parser: Parser0[Any], input: String, expected: String)(implicit
      loc: munit.Location
  ): Unit = {
    test(input) {
      parser
        .parseAll(input)
        .fold(
          e => assertEquals(e.show, expected),
          _ => fail("should not parse")
        )
    }
  }

  val ok = string("ok").void
  val nl = string("\n")
  val lx = (string("l") ~ digits).void
  val lxOk = ((lx | ok) ~ nl)

  // # Expectations:
  // OneOfStr
  error(
    string("foo") | string("bar") | string("baz"),
    "ko",
    s"""|ko
        |^
        |expectation:
        |* must match one of the strings: {"bar", "baz", "foo"}""".stripMargin
  )

  // InRange
  error(
    charIn(List('a', 'c', 'x', 'y')),
    "ko",
    """|ko
       |^
       |expectations:
       |* must be char: 'a'
       |* must be char: 'c'
       |* must be a char within the range of: ['x', 'y']""".stripMargin
  )

  // StartOfString
  error(
    ok ~ start,
    "ok",
    """|ok
       |  ^
       |expectation:
       |* must start the string""".stripMargin
  )

  // EndOfString
  error(
    ok,
    "okmore".stripMargin,
    """|okmore
       |  ^
       |expectation:
       |* must end the string""".stripMargin
  )

  // Length
  error(
    length(2),
    "a",
    """|a
       |^
       |expectation:
       |* must have a length of 2 but got a length of 1""".stripMargin
  )

  // ExpectedFailureAt
  error(
    not(ok),
    "okidou",
    """|okidou
       |^
       |expectation:
       |* must fail but matched with ok""".stripMargin
  )

  // Fail
  error(
    Fail,
    "ok",
    """|ok
       |^
       |expectation:
       |* must fail""".stripMargin
  )

  // FailWith
  error(
    failWith("error msg"),
    "ok",
    """|ok
       |^
       |expectation:
       |* must fail: error msg""".stripMargin
  )

  // WithContext
  error(
    withContext0(ok, "using ok") | withContext0(lx, "using lx"),
    "ko",
    """|ko
       |^
       |expectations:
       |* context: using ok, must match string: "ok"
       |* context: using lx, must be char: 'l'""".stripMargin
  )

  // Context
  error(
    lxOk.rep(9),
    """|l1
       |l2
       |l3
       |l4
       |ko
       |l6
       |l7
       |l8
       |l9
       |""".stripMargin,
    """|...
       |l3
       |l4
       |ko
       |^
       |expectations:
       |* must match string: "ok"
       |* must be char: 'l'
       |l6
       |l7
       |...""".stripMargin
  )

  error(
    lxOk.rep(3),
    """|l1
       |ko
       |l3""".stripMargin,
    """|l1
       |ko
       |^
       |expectations:
       |* must match string: "ok"
       |* must be char: 'l'
       |l3""".stripMargin
  )

  error(
    lxOk.rep(2),
    """|l1
       |ko""".stripMargin,
    """|l1
       |ko
       |^
       |expectations:
       |* must match string: "ok"
       |* must be char: 'l'""".stripMargin
  )

  error(
    lxOk.rep(2),
    """|ko
       |l2""".stripMargin,
    """|ko
       |^
       |expectations:
       |* must match string: "ok"
       |* must be char: 'l'
       |l2""".stripMargin
  )

  test("without input") {
    ok
      .parseAll("ko")
      .fold(
        e => {
          val expected =
            """|at offset 0
               |expectation:
               |* must match string: "ok"""".stripMargin
          assertEquals(Error(e.failedAtOffset, e.expected).show, expected)
        },
        _ => fail("should not parse")
      )
  }

  property("error show does not crash") {
    import cats.implicits._
    import ParserGen.{arbParser, arbString}

    forAll { (p: Parser[Unit], in: String) =>
      p.parseAll(in) match {
        case Right(_) => ()
        case Left(err) => assert(err.show ne null)
      }
    }
  }
}
