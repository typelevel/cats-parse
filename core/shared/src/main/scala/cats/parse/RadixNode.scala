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

import cats.syntax.all._

private[parse] final class RadixNode(
    protected val matched: String,
    protected val bitMask: Int,
    // the prefixes are the rest of the string after the fsts (not including the fsts Char)
    protected val prefixes: Array[String],
    protected val children: Array[RadixNode]
) {
  override def toString(): String = {
    def list[A](ary: Array[A]): String = ary.mkString("[", ", ", "]")
    val ps = list(prefixes)
    val cs = list(children)
    s"RadixNode($matched, $bitMask, $ps, $cs)"
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

  @tailrec
  final def matchAtOrNull(str: String, offset: Int): String =
    if (offset < str.length)  {
      val c = str.charAt(offset)
      // this is a hash of c
      val idx = c.toInt & bitMask
      val prefix = prefixes(idx)
      if (prefix ne null) {
        // accept the prefix fo this character
        if (str.regionMatches(offset, prefix, 0, prefix.length)) {
          children(idx).matchAtOrNull(str, offset + prefix.length)
        } else {
          matched
        }
      } else {
        matched
      }
    }
    else {
      matched
    }
}

private[parse] object RadixNode {
  private val emptyStringArray = new Array[String](1)
  private val emptyChildrenArray = new Array[RadixNode](1)

  private def fromNonEmpty(prefix: String, nonEmpty: NonEmptyList[String]): RadixNode = {
    val nonEmpties = nonEmpty.toList.filter(_.nonEmpty)
    val headKeys = nonEmpties.iterator.map(_.head).toSet

    @tailrec
    def findBitMask(b: Int): Int =
      if (b == 0xffff) b // biggest it can be
      else {
        val allDistinct = headKeys.iterator.map { c => c.toInt & b }.toSet.size == headKeys.size
        if (allDistinct) b
        else findBitMask((b << 1) | 1)
      }

    val bitMask = findBitMask((1 << Integer.highestOneBit(headKeys.size)) - 1)
    val branching = bitMask + 1
    val prefixes = new Array[String](branching)
    val children = new Array[RadixNode](branching)
    val tree = nonEmpties.groupByNel { s => (s.head.toInt & bitMask) }
    tree.foreach { case (idx, strings) =>
      // strings is a NonEmptyList[String] which all start with the same char
      val prefix1 = strings.reduce(commonPrefixSemilattice)
      val plen = prefix1.length
      val node = fromNonEmpty(prefix + prefix1, strings.map(_.drop(plen)))
      prefixes(idx) = prefix1
      children(idx) = node
    }

    // If nonEmpty contains the empty string, we have a valid prefix
    val thisPrefix = if (nonEmpty.exists(_.isEmpty)) prefix else null
    new RadixNode(thisPrefix, bitMask, prefixes, children)
  }

  def fromSortedStrings(strings: NonEmptyList[String]): RadixNode =
    fromNonEmpty("", strings)

  def fromStrings(strs: Iterable[String]): RadixNode =
    NonEmptyList.fromList(strs.toList.distinct) match {
      case Some(nel) => fromSortedStrings(nel)
      case None => empty
    }

  private val empty = new RadixNode(null, 0, emptyStringArray, emptyChildrenArray)

  final def commonPrefixLength(s1: String, s2: String): Int = {
    val len = Integer.min(s1.length, s2.length)
    var idx = 0
    var cont = true
    while (cont) {
      if ((idx >= len) || (s1.charAt(idx) != s2.charAt(idx))) {
        cont = false
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
