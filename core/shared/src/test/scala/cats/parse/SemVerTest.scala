/*
 * Copyright (c) 2020 Typelevel
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

class SemVerTest extends munit.ScalaCheckSuite {

  val genSemVer: Gen[String] = for {
    major <- Gen.choose(0, 10)
    minor <- Gen.choose(0, 10)
    patch <- Gen.choose(0, 100)
    preRelease <- Gen.oneOf("", "-alpha", "-beta", "-alpha.1")
    buildMetadata <- Gen.oneOf("", "+001", "+20130313144700", "+exp.sha.5114f85", "+21AF26D3")
  } yield s"$major.$minor.$patch$preRelease$buildMetadata"

  property("semver parses SemVer") {
    forAll(genSemVer) { (sv: String) =>
      assertEquals(SemVer.semverString.parseAll(sv), Right(sv))
    }
  }
}
