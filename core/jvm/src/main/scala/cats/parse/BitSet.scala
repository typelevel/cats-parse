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
