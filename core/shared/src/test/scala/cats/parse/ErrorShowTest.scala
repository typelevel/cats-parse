package cats.parse


import cats.implicits._

class ErrorShowTest extends munit.ScalaCheckSuite {

  def error(parser: Parser[Any], input: String, expected: String): Unit = {
    test(input) {
      parser.parseAll(input).fold(
        e => assert(
          e.show == expected,
          s"""|obtained:
              |${e.show}
              |
              |expected:
              |$expected""".stripMargin
        ),
        _ => fail("should not parse")
      )
    }
  }

  error(
    Parser.string("ok"),
    "ko",
    s"""|ko
        |^
        |one of: ["ok"]""".stripMargin
  )
}
