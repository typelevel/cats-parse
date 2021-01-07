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

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

class RepParserConstructionTest extends munit.ScalaCheckSuite {
  import ParserGen.biasSmall

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(1000)
      .withMaxDiscardRatio(10)

  val validMin = biasSmall(1)
  val validMin0 = biasSmall(0)
  val validMax = validMin

  val validMinMax = for {
    min <- validMin
    max <- biasSmall(min)
  } yield (min, max)

  val validMinMax0 = for {
    min <- validMin0
    max <- biasSmall(min)
  } yield (min, max)

  val invalidMin = Gen.choose(Int.MinValue, 0)
  val invalidMin0 = Gen.choose(Int.MinValue, -1)
  val invalidMax = Gen.choose(Int.MinValue, 0)

  val invalidMinMax = Gen.oneOf(
    for (min <- validMin; maxDiff <- biasSmall(1))
      yield (min, Integer.min(min - maxDiff, Int.MinValue)),
    for (min <- invalidMin; max <- biasSmall(0)) yield (min, max),
    for (min <- invalidMin; max <- invalidMax) yield (min, max),
    for (min <- validMin; max <- invalidMax) yield (min, max)
  )

  val invalidMinMax0 = Gen.oneOf(
    for (min <- validMin0; maxDiff <- biasSmall(1))
      yield (min, Integer.min(min - maxDiff, Int.MinValue)),
    for (min <- invalidMin0; max <- biasSmall(0)) yield (min, max),
    for (min <- invalidMin0; max <- invalidMax) yield (min, max),
    for (min <- validMin0; max <- invalidMax) yield (min, max)
  )

  property("rep constructs parser with min >= 1, min <= max") {
    forAll(validMinMax) {
      case (min: Int, max: Int) => {
        Parser.anyChar.rep(min = min, max = max)
        assert(true)
      }
    }
  }

  property("rep fails to construct parser without min >= 1, min <= max") {
    forAll(invalidMinMax) {
      case (min: Int, max: Int) => {
        intercept[IllegalArgumentException] {
          Parser.anyChar.rep(min = min, max = max)
        }
        intercept[IllegalArgumentException] {
          Parser.repSep(Parser.anyChar, min = min, max = max, Parser.pure(""))
        }
        assert(true)
      }
    }
  }

  property("rep constructs parser with min >= 1") {
    forAll(validMin) { (min: Int) =>
      {
        Parser.anyChar.rep(min = min)
        assert(true)
      }
    }
  }

  property("rep fails to construct parser without min >= 1") {
    forAll(invalidMin) { (min: Int) =>
      {
        intercept[IllegalArgumentException] {
          Parser.anyChar.rep(min = min)
        }
        assert(true)
      }
    }
  }

  property("rep0 constructs parser with min >= 0, (min, 1) <= max") {
    forAll(validMinMax0) {
      case (min: Int, max: Int) => {
        Parser.anyChar.rep0(min = min, max = max)
        assert(true)
      }
    }
  }

  property("rep0 fails to construct parser without min >= 0, (min, 1) <= max") {
    forAll(invalidMinMax0) {
      case (min: Int, max: Int) => {
        intercept[IllegalArgumentException] {
          Parser.anyChar.rep0(min = min, max = max)
        }
        intercept[IllegalArgumentException] {
          Parser.repSep0(Parser.anyChar, min = min, max = max, Parser.pure(""))
        }
        assert(true)
      }
    }
  }

  property("rep0 constructs parser with min >= 0") {
    forAll(validMin0) { (min: Int) =>
      {
        Parser.anyChar.rep0(min = min)
        assert(true)
      }
    }
  }

  property("rep0 fails to construct parser without min >= 0") {
    forAll(invalidMin0) { (min: Int) =>
      {
        intercept[IllegalArgumentException] {
          Parser.anyChar.rep0(min = min)
        }
        assert(true)
      }
    }
  }

  property("repAs constructs parser with min >= 1, min <= max") {
    forAll(validMinMax) {
      case (min: Int, max: Int) => {
        Parser.anyChar.repAs[String](min = min, max = max)
        assert(true)
      }
    }
  }

  property("repAs fails to construct parser without min >= 1, min <= max") {
    forAll(invalidMinMax) {
      case (min: Int, max: Int) => {
        intercept[IllegalArgumentException] {
          Parser.anyChar.repAs[String](min = min, max = max)
        }
        assert(true)
      }
    }
  }

  property("repAs constructs parser with min >= 1") {
    forAll(validMin) { (min: Int) =>
      {
        Parser.anyChar.repAs[String](min = min)
        assert(true)
      }
    }
  }

  property("repAs fails to construct parser without min >= 1") {
    forAll(invalidMin) { (min: Int) =>
      {
        intercept[IllegalArgumentException] {
          Parser.anyChar.repAs[String](min = min)
        }
        assert(true)
      }
    }
  }

  property("repAs0 constructs parser with max >= 1") {
    forAll(validMax) { (max: Int) =>
      {
        Parser.anyChar.repAs0[String](max = max)
        assert(true)
      }
    }
  }

  property("repAs0 fails to construct parser without max >= 1") {
    forAll(invalidMax) { (max: Int) =>
      {
        intercept[IllegalArgumentException] {
          Parser.anyChar.repAs0[String](max = max)
        }
        assert(true)
      }
    }
  }

}
