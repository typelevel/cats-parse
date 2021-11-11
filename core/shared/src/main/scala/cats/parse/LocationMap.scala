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

import java.util.Arrays

/** This is a class to convert linear offset in a string into lines, or the column and line numbers.
  *
  * This is useful for display to humans who in text editors think in terms of line and column
  * numbers
  */
class LocationMap(val input: String) {

  private[this] val lines: Array[String] =
    input.split("\n", -1)

  private[this] val endsWithNewLine: Boolean =
    (input.length > 0) && (input.last == '\n')

  // The position of the first element of the ith line
  private[this] val firstPos: Array[Int] = {
    val it = lines.iterator.map(_.length)
    val it2 = new Iterator[(Int, Boolean)] {
      def hasNext = it.hasNext
      def next() = {
        val hn = hasNext
        val i = it.next()
        (i, hn)
      }
    }
    it2
      .map {
        case (i, true) => i + 1 // add 1 for the newline
        case (i, false) => i
      }
      .toArray
      .scanLeft(0)(_ + _)
  }

  /** How many lines are there
    */
  def lineCount: Int = lines.length

  /** Given a string offset return the line and column If input.length is given (EOF) we return the
    * same value as if the string were one character longer (i.e. if we have appended a non-newline
    * character at the EOF)
    */
  def toLineCol(offset: Int): Option[(Int, Int)] =
    if (offset < 0 || offset > input.length) None
    else {
      val Caret(_, row, col) = toCaretUnsafe(offset)
      Some((row, col))
    }

  /** Convert an offset to a Caret.
    * @throws IllegalArgumentException
    *   if offset is longer than input
    */
  def toCaretUnsafe(offset: Int): Caret =
    if (offset < 0 || offset > input.length)
      throw new IllegalArgumentException(s"offset = $offset exceeds ${input.length}")
    else if (offset == input.length) {
      // this is end of line
      if (offset == 0) Caret.Start
      else {
        val Caret(_, line, col) = toCaretUnsafe(offset - 1)
        if (endsWithNewLine) Caret(offset, line + 1, 0)
        else Caret(offset, line, col + 1)
      }
    } else {
      val idx = Arrays.binarySearch(firstPos, offset)
      if (idx < 0) {
        // idx = (~(insertion pos) - 1)
        // The insertion point is defined as the point at which the key would be
        // inserted into the array: the index of the first element greater than
        // the key, or a.length if all elements in the array are less than the specified key.
        //
        // so insertion pos = ~(idx + 1)
        val row = ~(idx + 1)
        // so we are pointing into a row
        val rowStart = firstPos(row)
        val col = offset - rowStart
        Caret(offset, row, col)
      } else {
        // idx is exactly the right value because offset is beginning of a line
        Caret(offset, idx, 0)
      }
    }

  def toCaret(offset: Int): Option[Caret] =
    if (offset < 0 || offset > input.length) None
    else Some(toCaretUnsafe(offset))

  /** return the line without a newline
    */
  def getLine(i: Int): Option[String] =
    if (i >= 0 && i < lines.length) Some(lines(i))
    else None

  /** Return the offset for a given row/col. if we return Some(input.length) this means EOF if we
    * return Some(i) for 0 <= i < input.length it is a valid item else offset < 0 or offset >
    * input.length we return None
    */
  def toOffset(line: Int, col: Int): Option[Int] =
    if ((line < 0) || (line > lines.length)) None
    else Some(firstPos(line) + col)
}

object LocationMap {
  def apply(str: String): LocationMap = new LocationMap(str)
}
