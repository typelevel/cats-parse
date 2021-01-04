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

class AccumulatorTest extends munit.ScalaCheckSuite {
  property("intCounter counts how many times it's invoked") {
    forAll(Gen.choose(0, 1000)) { (n: Int) =>
      {
        val intAppender = implicitly[Accumulator0[Any, Int]].newAppender()
        for (_ <- 1 to n) intAppender.append(())
        assertEquals(intAppender.finish(), n)
      }
    }
  }

  test("intCounters newAppender returns a new appender") {
    val counter = implicitly[Accumulator0[Any, Int]]
    val intAppender1 = counter.newAppender()
    val intAppender2 = counter.newAppender()
    val n = 100
    for (_ <- 1 to n) {
      intAppender1.append(())
      intAppender2.append(())
    }
    assert(intAppender1.finish() == n)
    assert(intAppender2.finish() == n)
  }
}
