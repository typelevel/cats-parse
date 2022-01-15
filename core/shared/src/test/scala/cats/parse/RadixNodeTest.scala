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

import org.scalacheck.Prop
import org.scalacheck.Prop.forAll

class RadixNodeTest extends munit.ScalaCheckSuite {
  val tests: Int = if (BitSetUtil.isScalaJs) 50 else 20000

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(tests)
      .withMaxDiscardRatio(10)

  property("commonPrefixLength is consistent") {
    forAll { (s1: String, s2: String) =>
      val len = RadixNode.commonPrefixLength(s1, s2)
      val minLen = Integer.min(s1.length, s2.length)

      assert(len >= 0)
      assert(len <= minLen)
      assertEquals(s1.take(len), s2.take(len))
      if (len < minLen) assertNotEquals(s1.charAt(len), s2.charAt(len))
    }
  }

  property("commonPrefixLength is commutative") {
    forAll { (s1: String, s2: String) =>
      assertEquals(RadixNode.commonPrefixLength(s1, s2), RadixNode.commonPrefixLength(s2, s1))
    }
  }

  property("commonPrefixLength(s, s + r) == s.length") {
    forAll { (s: String, r: String) =>
      assert(RadixNode.commonPrefixLength(s, s + r) == s.length)
    }
  }

  property("commonPrefixLength(s + r, s + t) >= s.length") {
    forAll { (s: String, r: String, t: String) =>
      assert(RadixNode.commonPrefixLength(s + r, s + t) >= s.length)
    }
  }

  property("commonPrefixLength is commutative") {
    forAll { (s: String, r: String) =>
      assertEquals(RadixNode.commonPrefixLength(s, r), RadixNode.commonPrefixLength(r, s))
    }
  }

  property("If we match, then string is in the set") {
    def law(ss0: List[String], target: String): Prop = {
      val ss = ss0.filter(_.nonEmpty)
      val radix = RadixNode.fromStrings(ss)
      assertEquals(radix.matchAt(target, 0) >= 0, ss.exists(target.startsWith(_)))
    }

    val p1 = forAll { (ss: List[String], head: Char, tail: String) =>
      val target = s"$head$tail"
      law(ss, target)
    }

    val regressions =
      (List("噈"), s"噈\u0000") ::
        Nil

    regressions.foldLeft(p1) { case (p, (ss, t)) => p && law(ss, t) }
  }

  property("If we match everything in the set") {
    forAll { (ss0: List[String], head: Char, tail: String) =>
      val s1 = s"$head$tail"
      val ss = s1 :: ss0
      val radix = RadixNode.fromStrings(ss)
      ss.foreach { target =>
        assert((radix.matchAt(target, 0) >= 0) || target.isEmpty)
      }
    }
  }
}
