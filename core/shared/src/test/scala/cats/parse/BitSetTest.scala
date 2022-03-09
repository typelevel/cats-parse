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

class BitSetTest extends munit.ScalaCheckSuite {
  // TODO: Remove isScalaJs/isScalaJvm in next minor version update. See https://github.com/typelevel/cats-parse/issues/391.
  test("isScalaJs/isScalaJvm is consistent") {
    // This will need to be updated if we ever add scala-native
    assert(!(BitSetUtil.isScalaJs && BitSetUtil.isScalaJvm))
    assert(BitSetUtil.isScalaJs || BitSetUtil.isScalaJvm)
    assert(BitSetUtil.isScalaJs ^ BitSetUtil.isScalaJvm)
  }

  property("BitSetUtil union works") {
    forAll { (cs: List[List[Char]]) =>
      val arys = cs.iterator.filter(_.nonEmpty).map(_.toArray.sorted)
      val bs = arys.map { ary => (ary(0).toInt, BitSetUtil.bitSetFor(ary)) }
      val sortedFlat = BitSetUtil.union(bs)
      assertEquals(sortedFlat.toSet, cs.flatten.toSet)
    }
  }

  property("BitSet.isSingleton is correct") {
    forAll { (c0: Char, cs: Set[Char]) =>
      val set = cs + c0
      val bs = BitSetUtil.bitSetFor(set.toArray.sorted)

      assertEquals(BitSetUtil.isSingleton(bs), set.size == 1)
    }
  }
}
