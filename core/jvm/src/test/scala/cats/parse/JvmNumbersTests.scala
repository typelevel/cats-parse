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

import org.scalacheck.Prop.forAll
import org.typelevel.jawn.ast.{JNum, JParser}

/** Jawn doesn't publish artifacts for all the versions we support we use jawn to test JSON parsing
  * methods
  */
class JvmNumbersTest extends munit.ScalaCheckSuite {
  val tests: Int = 20000

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(tests)
      .withMaxDiscardRatio(10)

  def jawnLaw(a: String) = {
    // 2.11 doesn't have toOption
    val jn = Numbers.jsonNumber.parseAll(a) match {
      case Left(_) => None
      case Right(a) => Some(a)
    }
    val jawn = JParser.parseFromString(a).toOption.collect { case jnum: JNum => jnum }

    assertEquals(jn.isDefined, jawn.isDefined)

    if (jn.isDefined) {
      assertEquals(jn.get, a)
      assertEquals(BigDecimal(a), jawn.get.asBigDecimal)
    }
  }
  property("jsonNumber parses if and only if Jawn would parse it as a number") {
    forAll { (a: String) => jawnLaw(a) }
  }

  property("jsonNumber parses if and only if Jawn would parse it as a number (valid Double)") {
    forAll { (a: Double) =>
      if (a.isNaN || a.isInfinite) ()
      else jawnLaw(a.toString)
    }
  }

  property("jsonNumber parses if and only if Jawn would parse it as a number (valid BigDecimal)") {
    forAll { (a: BigDecimal) => jawnLaw(a.toString) }
  }

  property("jsonNumber parses if and only if Jawn would parse it as a number (valid Int)") {
    forAll { (a: Int) => jawnLaw(a.toString) }
  }
}
