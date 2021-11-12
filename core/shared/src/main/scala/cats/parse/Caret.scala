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

import cats.Order

/** This is a pointer to a zero based line, column, and total offset.
  */
case class Caret(line: Int, col: Int, offset: Int)

object Caret {
  val Start: Caret = Caret(0, 0, 0)

  implicit val caretOrder: Order[Caret] =
    new Order[Caret] {
      def compare(left: Caret, right: Caret): Int = {
        val c0 = Integer.compare(left.line, right.line)
        if (c0 != 0) c0
        else {
          val c1 = Integer.compare(left.col, right.col)
          if (c1 != 0) c1
          else Integer.compare(left.offset, right.offset)
        }
      }
    }

  implicit val caretOrdering: Ordering[Caret] =
    caretOrder.toOrdering
}
