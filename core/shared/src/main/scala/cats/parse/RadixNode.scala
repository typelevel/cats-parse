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
import java.util.Arrays
import scala.annotation.tailrec

private[parse] final class RadixNode(
    protected val fsts: Array[Char],
    // the prefixes are the rest of the string after the fsts (not including the fsts Char)
    protected val prefixes: Array[String],
    protected val children: Array[RadixNode],
    protected val word: Boolean
) {
  override def toString(): String =
    s"RadixNode(${fsts.mkString("[", ", ", "]")}, ${children.mkString("[", ", ", "]")}, $word)"

  /** If this matches, return the new offset, else return -1
    *
    * @param str
    *   the string to match against this RadixNode
    * @param offset
    *   the initial offset
    * @return
    *   the new offset after a match, or -1
    */
  def matchAt(str: String, off: Int): Int = {
    val strLength = str.length
    var offset = off
    var tree = this
    var cont = offset < strLength
    var lastMatch = -1

    while (cont) {
      val c = str.charAt(offset)
      val idx = Arrays.binarySearch(tree.fsts, c)
      if (idx >= 0) {
        val prefix = tree.prefixes(idx)
        // accept the prefix fo this character
        if (str.startsWith(prefix, offset + 1)) {
          val children = tree.children(idx)
          offset += (prefix.length + 1)
          tree = children
          cont = offset < strLength
          if (children.word) lastMatch = offset
        } else {
          cont = false
        }
      } else {
        cont = false
      }
    }

    lastMatch
  }
}

private[parse] object RadixNode {
  @tailrec
  private def groupByNonEmptyPrefix(
      keys: List[String],
      prefix: String,
      current: NonEmptyList[String],
      acc: List[(Char, String, NonEmptyList[String])]
  ): List[(Char, String, NonEmptyList[String])] =
    keys match {
      case key :: keys =>
        val prefixSize = commonPrefixLength(prefix, key)
        if (prefixSize == 0) {
          // no common prefix, group current suffixes together sorted again
          groupByNonEmptyPrefix(
            keys,
            key,
            NonEmptyList.one(key),
            (prefix(0), prefix.tail, current.map(_.drop(prefix.size)).reverse) :: acc
          )
        } else {
          // clip the prefix to the length, and continue
          groupByNonEmptyPrefix(keys, prefix.take(prefixSize), key :: current, acc)
        }
      case Nil =>
        (prefix(0), prefix.tail, current.map(_.drop(prefix.size)).reverse) :: acc
    }

  def fromSortedStrings(strings: NonEmptyList[String]): RadixNode =
    NonEmptyList.fromList(strings.filter(_.nonEmpty)) match {
      case Some(nonEmpty) =>
        val grouped =
          groupByNonEmptyPrefix(
            nonEmpty.tail,
            nonEmpty.head,
            NonEmptyList.one(nonEmpty.head),
            Nil
          ).reverse
            .map { case (fst, prefix, v) => (fst, prefix, fromSortedStrings(v)) }

        val (fsts, prefixes, children) = grouped.unzip3

        new RadixNode(
          fsts.toArray,
          prefixes.toArray,
          children.toArray,
          nonEmpty.size < strings.size
        )
      case None =>
        leaf
    }

  def fromStrings(strs: Iterable[String]): RadixNode =
    NonEmptyList.fromList(strs.toList.distinct.sorted) match {
      case Some(nel) => fromSortedStrings(nel)
      case None => leaf
    }

  private val leaf = new RadixNode(Array.empty, Array.empty, Array.empty, true)

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
}
