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
import org.scalacheck.Gen
import org.typelevel.jawn.ast.{JString, JParser}
import scala.util.Success
import org.scalacheck.Arbitrary

/** Jawn doesn't publish artifacts for all the versions we support we use jawn to test JSON parsing
  * methods
  */
class JvmStringsTest extends munit.ScalaCheckSuite {
  val tests: Int = 200000

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(tests)
      .withMaxDiscardRatio(10)

  property("JString(str).render parses") {
    forAll { (a: String) =>
      val res = strings.Json.delimited.parser.parseAll(JString(a).render())
      assertEquals(res, Right(a))
    }
  }

  property("Strings.jsonEscape(str) parses in Jawn") {
    forAll { (a: String) =>
      val json = strings.Json.delimited.encode(a)
      val res = JParser.parseFromString(json).get
      assertEquals(res, JString(a))
    }
  }

  property("jsonString parses in exactly the same cases as Jawn") {
    val genBase = Gen.oneOf(Arbitrary.arbitrary[String], Gen.identifier)
    val maybeEscaped = Gen.oneOf(genBase, genBase.map(strings.Json.delimited.encode(_)))

    forAll(maybeEscaped) { (raw: String) =>
      val resJawn = JParser
        .parseFromString(raw) match {
        case Success(JString(str)) => Some(str)
        case _ => None
      }

      val resThis = strings.Json.delimited.parser.parseAll(raw) match {
        case Right(r) => Some(r)
        case Left(_) => None
      }
      assertEquals(resJawn, resThis)
    }
  }
}
