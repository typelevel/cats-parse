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

  val ok = string("ok")
  val nl = string("\n")
  val lx = string("l") ~ digits
  val lxOk = ((lx | ok) ~ nl)

  // # Expectations:
  // OneOfStr
  error(
    string("foo") | string("bar") | string("baz"),
    "ko",
    s"""|ko
        |^
        |* one of: {"bar", "baz", "foo"}""".stripMargin
  )

  // InRange
  error(
    charIn(List('a', 'c', 'x', 'y')),
    "ko",
    """|ko
       |^
       |* is: a
       |* is: c
       |* in range: [x, y]""".stripMargin
  )

  // StartOfString
  error(
    ok ~ start,
    "ok",
    """|ok
       |  ^
       |* start of string""".stripMargin
  )

  // EndOfString
  error(
    lx.repExactlyAs[Int](2),
    "l1l2l3".stripMargin,
    """|l1l2l3
       |    ^
       |* end of string, length = 6""".stripMargin
  )

  // Length
  error(
    length(2),
    "a",
    """|a
       |^
       |* length: expected = 2, actual = 1""".stripMargin
  ) // this does repport the correct error IMO

  // ExpectedFailureAt
  error(
    not(ok),
    "okidou",
    """|okidou
       |^
       |* failure at ok""".stripMargin
  )

  // Fail
  error(
    ok ~ failWith("ko"),
    "ok",
    """|ok
       |  ^
       |* fail: ko""".stripMargin
  )

  // FailWith
  error(
    failWith("error msg"),
    "ok",
    """|ok
       |^
       |* fail: error msg""".stripMargin
  )

  // WithContext
  error(
    withContext0(ok, "ok") | withContext0(lx, "lx"),
    "ko",
    """|ko
       |^
       |* ok, one of: {"ok"}
       |* lx, is: l""".stripMargin
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
       |* one of: {"ok"}
       |* is: l
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
       |* one of: {"ok"}
       |* is: l
       |l3""".stripMargin
  )

  error(
    lxOk.rep(2),
    """|l1
       |ko""".stripMargin,
    """|l1
       |ko
       |^
       |* one of: {"ok"}
       |* is: l""".stripMargin
  )

  error(
    lxOk.rep(2),
    """|ko
       |l2""".stripMargin,
    """|ko
       |^
       |* one of: {"ok"}
       |* is: l
       |l2""".stripMargin
  )

  property("error show does not crash") {
    import cats.implicits._
    import ParserGen.{arbParser0, arbParser, arbString}

    forAll { (p: Parser[Any], in: String) =>
      p.parseAll(in) match {
        case Right(_) => ()
        case Left(err) => assert(err.show ne null)
      }
    }
  }
}
