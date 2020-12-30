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

class RadixNodeTest extends munit.ScalaCheckSuite {
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
}
