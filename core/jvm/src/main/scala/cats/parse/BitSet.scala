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

import java.util.BitSet

object BitSetUtil {
  type Tpe = BitSet

  @inline final val isScalaJs = false
  @inline final val isScalaJvm = true

  @inline final def isSet(b: BitSet, idx: Int): Boolean =
    // BitSet can't deal with negatives, so mask those out
    b.get(idx & Int.MaxValue)

  def bitSetFor(charArray: Array[Char]): BitSet = {
    val min = charArray(0).toInt
    val bs = new BitSet(charArray(charArray.length - 1).toInt + 1 - min)
    var idx = 0
    while (idx < charArray.length) {
      bs.set(charArray(idx).toInt - min)
      idx += 1
    }

    bs
  }

  // what are all the Chars in these bitsets
  def union(bs: List[(Int, BitSet)]): Iterable[Char] = {
    def toIter(m: Int, bs: BitSet): Iterator[Char] =
      Iterator.iterate(0) { m => bs.nextSetBit(m + 1) }
        .takeWhile(_ >= 0)
        .map { i => (m + i).toChar }

    bs.flatMap { case (m, bs) => toIter(m, bs) }
  }
}
