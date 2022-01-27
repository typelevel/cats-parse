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

import org.scalacheck.{Gen, Prop}
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
      val matchLen = radix.matchAt(target, 0)
      assertEquals(
        matchLen >= 0,
        ss.exists(target.startsWith(_)),
        s"ss=$ss, ss.size = ${ss.size}, matchLen=$matchLen, radix=$radix"
      )
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

  property("we match everything in the set") {
    forAll { (ss0: List[String], head: Char, tail: String) =>
      val s1 = s"$head$tail"
      val ss = s1 :: ss0
      val radix = RadixNode.fromStrings(ss)
      ss.foreach { target =>
        assert((radix.matchAt(target, 0) >= 0) || target.isEmpty)
      }
    }
  }

  property("commonPrefix is associative") {
    val sl = RadixNode.commonPrefixSemilattice
    forAll { (s0: String, s1: String, s2: String) =>
      val left = sl.combine(sl.combine(s0, s1), s2)
      val right = sl.combine(s0, sl.combine(s1, s2))
      assertEquals(left, right)
    }
  }

  property("commonPrefix commutes") {
    val sl = RadixNode.commonPrefixSemilattice
    forAll { (s0: String, s1: String) =>
      val left = sl.combine(s0, s1)
      val right = sl.combine(s1, s0)
      assertEquals(left, right)
    }
  }

  property("commonPrefix is finds prefix") {
    val sl = RadixNode.commonPrefixSemilattice
    forAll { (s0: String, suffix: String) =>
      assertEquals(sl.combine(s0, s0 + suffix), s0)
      assertEquals(sl.combine(s0 + suffix, s0), s0)
    }
  }

  property("RadixNode.fromStrings(emptyString :: Nil) matches everything") {
    val nilRadix = RadixNode.fromStrings("" :: Nil)
    forAll { (targ: String) =>
      forAll(Gen.choose(0, targ.length)) { off =>
        assertEquals(nilRadix.matchAtOrNull(targ, off), "")
      }
    }
  }

  property("fromString(Nil) matches nothing") {
    forAll { (s: String) =>
      forAll(Gen.choose(-1, s.length + 1)) { off =>
        assert(RadixNode.fromStrings(Nil).matchAt(s, off) < 0)
      }
    }
  }

  property("RadixTree singleton") {
    forAll { (s: String, prefix: String, suffix: String) =>
      val tree = RadixNode.fromStrings(s :: Nil)
      assertEquals(tree.matchAtOrNull(prefix + s + suffix, prefix.length), s)
    }
  }

  property("RadixTree union property") {
    forAll { (t1: List[String], t2: List[String], targ: String) =>
      val tree1 = RadixNode.fromStrings(t1)
      val tree2 = RadixNode.fromStrings(t2)
      val tree3 = RadixNode.fromStrings(t1 ::: t2)

      forAll(Gen.choose(-1, targ.length + 1)) { off =>
        val m1 = math.max(tree1.matchAt(targ, off), tree2.matchAt(targ, off))
        assertEquals(m1, tree3.matchAt(targ, off))
      }
    }
  }

  property("matchAtOrNull is consistent") {
    forAll { (args: List[String], targ: String) =>
      val radix = RadixNode.fromStrings(args)
      forAll(Gen.choose(-1, targ.length + 1)) { off =>
        radix.matchAt(targ, off) match {
          case x if x < 0 =>
            assertEquals(radix.matchAtOrNull(targ, off), null)
          case len =>
            val left = radix.matchAtOrNull(targ, off)
            assert(off + len <= targ.length, s"len = $len, off = $off")
            assertEquals(left, targ.substring(off, off + len), s"len = $len, left = $left")
        }
      }
    }
  }

  test("example from ParserTest") {
    val tree = RadixNode.fromStrings(List("foo", "foobar", "foofoo", "foobat"))
    assertEquals(tree.matchAtOrNull("foobal", 0), "foo")
  }

  property("RadixNode.allStrings roundTrips") {
    forAll { (ss: List[String]) =>
      assertEquals(RadixNode.fromStrings(ss).allStrings.sorted, ss.distinct.sorted)
    }
  }
}
