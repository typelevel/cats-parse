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

import cats.data.NonEmptyList
import cats.kernel.Semilattice
import scala.annotation.tailrec

private[parse] final class RadixNode(
    matched: String,
    bitMask: Int,
    // the prefixes are the rest of the string after the fsts (not including the fsts Char)
    prefixes: Array[String],
    children: Array[RadixNode]
) {
  override def toString(): String = {
    def list[A](ary: Array[A]): String = ary.mkString("[", ", ", "]")
    val ps = list(prefixes)
    val cs = list(children)
    s"RadixNode($matched, $bitMask, $ps, $cs)"
  }

  /** @return
    *   all strings that are in this RadixNode
    */
  def allStrings: List[String] = {
    // matched may be null (meaning there is not yet a partial match)
    // or it may be non-null: there is a partial match
    // if it is non-null it may be duplicated in children
    val rest = children.iterator.flatMap {
      case null => Nil
      case c => c.allStrings
    }

    if (matched eq null) rest.toList
    else (matched :: rest.filterNot(_ == matched).toList)
  }

  /** If this matches, return the new offset, else return -1
    *
    * @param str
    *   the string to match against this RadixNode
    * @param offset
    *   the initial offset
    * @return
    *   the new offset after a match, or -1
    */
  def matchAt(str: String, off: Int): Int =
    matchAtOrNull(str, off) match {
      case null => -1
      case nonNull => nonNull.length
    }

  final def matchAtOrNull(str: String, offset: Int): String =
    if (offset < 0) null
    else matchAtOrNullLoop(str, offset)

  // loop invariant: 0 <= offset
  @tailrec
  final protected def matchAtOrNullLoop(str: String, offset: Int): String =
    if (offset < str.length) {
      val c = str.charAt(offset)
      // this is a hash of c
      val idx = c.toInt & bitMask
      val prefix = prefixes(idx)
      if (prefix ne null) {
        // accept the prefix fo this character
        val plen = prefix.length
        if (str.regionMatches(offset, prefix, 0, plen)) {
          children(idx).matchAtOrNullLoop(str, offset + plen)
        } else {
          matched
        }
      } else {
        matched
      }
    } else if (offset > str.length) {
      null
    } else {
      // this is only the case where offset == str.length
      matched
    }
}

private[parse] object RadixNode {
  private val emptyStringArray = new Array[String](1)
  private val emptyChildrenArray = new Array[RadixNode](1)

  private def fromTree(prevMatch: String, prefix: String, rest: List[String]): RadixNode = {
    val nonEmpties = rest.filter(_.nonEmpty)

    // If nonEmpty contains the empty string, we have a valid prefix
    val thisPrefix = if (rest.exists(_.isEmpty)) prefix else prevMatch

    if (nonEmpties.isEmpty) {
      new RadixNode(thisPrefix, 0, emptyStringArray, emptyChildrenArray)
    } else {
      val headKeys = nonEmpties.iterator.map(_.head).toSet
      @tailrec
      def findBitMask(b: Int): Int =
        if (b == 0xffff) b // biggest it can be
        else {
          val allDistinct = headKeys.iterator.map { c => c.toInt & b }.toSet.size == headKeys.size
          if (allDistinct) b
          else findBitMask((b << 1) | 1)
        }

      val bitMask = findBitMask(0)
      val branching = bitMask + 1
      val prefixes = new Array[String](branching)
      val children = new Array[RadixNode](branching)
      val tree = nonEmpties.groupBy { s => (s.head.toInt & bitMask) }
      tree.foreach { case (idx, strings) =>
        // strings is a NonEmptyList[String] which all start with the same char
        val prefix1 = strings.reduce(commonPrefixSemilattice.combine(_, _))
        val plen = prefix1.length
        val s1 = if (plen == 0) strings else strings.map(_.drop(plen))
        val node = fromTree(thisPrefix, prefix + prefix1, s1)
        prefixes(idx) = prefix1
        children(idx) = node
      }

      new RadixNode(thisPrefix, bitMask, prefixes, children)
    }
  }

  /** This is identical to fromStrings and only here for binary compatibility
    */
  def fromSortedStrings(strings: NonEmptyList[String]): RadixNode =
    fromTree(null, "", strings.toList.distinct)

  def fromStrings(strs: Iterable[String]): RadixNode =
    fromTree(null, "", strs.toList.distinct)

  final def commonPrefixLength(s1: String, s2: String): Int = {
    val len = Integer.min(s1.length, s2.length)
    var idx = 0
    while (idx < len) {
      if (s1.charAt(idx) != s2.charAt(idx)) {
        return idx
      } else {
        idx = idx + 1
      }
    }

    idx
  }

  val commonPrefixSemilattice: Semilattice[String] =
    new Semilattice[String] {
      def combine(x: String, y: String): String = {
        val l = commonPrefixLength(x, y)
        if (l == 0) ""
        else if (l == x.length) x
        else if (l == y.length) y
        else x.take(l)
      }
    }
}
