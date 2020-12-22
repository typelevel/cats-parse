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

import cats.data.NonEmptyList

import scala.annotation.tailrec

class RadixNode(
    val fsts: Array[Char],
    val prefixes: Array[String],
    val children: Array[RadixNode],
    val word: Boolean
) {
  override def toString(): String =
    s"RadixNode(${fsts.mkString("[", ", ", "]")}, ${children.mkString("[", ", ", "]")}, $word)"
}

object RadixNode {
  def fromSortedStrings(strings: NonEmptyList[String]): RadixNode = {
    @tailrec
    def groupByNonEmptyPrefix(
        keys: List[String],
        prefix: String,
        current: NonEmptyList[String],
        acc: List[(Char, String, NonEmptyList[String])]
    ): List[(Char, String, NonEmptyList[String])] =
      keys match {
        case key :: keys =>
          val prefixSize = commonPrefix(prefix, key)
          if (prefixSize == 0) {
            // no common prefix, group current suffixes together sorted again
            groupByNonEmptyPrefix(
              keys,
              key,
              NonEmptyList.one(key),
              (prefix(0), prefix, current.map(_.drop(prefix.size)).reverse) :: acc
            )
          } else {
            // clip the prefix to the length, and continue
            groupByNonEmptyPrefix(keys, prefix.take(prefixSize), key :: current, acc)
          }
        case Nil =>
          (prefix(0), prefix, current.map(_.drop(prefix.size)).reverse) :: acc
      }
    NonEmptyList.fromList(strings.filter(_.nonEmpty)) match {
      case Some(nonEmpty) =>
        val grouped =
          groupByNonEmptyPrefix(
            nonEmpty.tail,
            nonEmpty.head,
            NonEmptyList.one(nonEmpty.head),
            Nil
          ).reverse.map { case (fst, prefix, v) => (fst, prefix, fromSortedStrings(v)) }
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
  }

  private val leaf = new RadixNode(Array.empty, Array.empty, Array.empty, true)

  private def commonPrefix(s1: String, s2: String): Int = {
    @tailrec
    def loop(idx: Int): Int =
      if (idx >= s1.size || idx >= s2.size) {
        idx
      } else {
        val c1 = s1(idx)
        val c2 = s2(idx)
        if (c1 == c2) {
          loop(idx + 1)
        } else {
          idx
        }
      }
    loop(0)
  }
}
