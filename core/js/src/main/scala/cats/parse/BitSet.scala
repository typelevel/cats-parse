package cats.parse

import scala.collection.mutable.BitSet

object BitSetUtil {
  type Tpe = BitSet

  @inline final val isScalaJs = true
  @inline final val isScalaJvm = false

  @inline final def isSet(b: BitSet, idx: Int): Boolean =
    (idx >= 0) && b(idx)

  def bitSetFor(charArray: Array[Char]): BitSet = {
    val min = charArray(0).toInt
    val bs = new BitSet(charArray(charArray.length - 1).toInt + 1 - min)
    var idx = 0
    while (idx < charArray.length) {
      bs += charArray(idx).toInt - min
      idx += 1
    }

    bs
  }

  // what are all the Chars in these bitsets
  def union(bs: List[(Int, BitSet)]): Iterable[Char] = {
    def toIter(m: Int, bs: BitSet): Iterator[Char] =
      bs.iterator.map { i => (i + m).toChar } ++ Iterator.single(m.toChar)

    bs.flatMap { case (m, bs) => toIter(m, bs) }
  }
}
