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

import org.scalacheck.{Arbitrary, Gen, Prop}
import Prop.forAll

class LocationMapTest extends munit.ScalaCheckSuite {

  property("single line locations") {
    val singleLine: Gen[String] =
      Arbitrary.arbitrary[String].map(_.filterNot(_ == '\n'))

    forAll(singleLine, Arbitrary.arbitrary[Int]) { (sline, offset) =>
      val lm = LocationMap(sline)

      assert(lm.getLine(0) == Some(sline))
      lm.toLineCol(offset) match {
        case None =>
          assert(offset < 0 || offset >= sline.length)
        case Some((row, col)) =>
          assert(row == 0)
          assert(col == offset)
      }
    }
  }

  property("position of end-of-line is the same as adding a constant") {
    forAll { (str: String) =>
      val lm0 = LocationMap(str)
      val lm1 = LocationMap(str + "a")

      assert(lm0.toLineCol(str.length) == lm1.toLineCol(str.length))
    }
  }

  test("some specific examples") {
    val lm0 = LocationMap("\n")
    assert(lm0.toLineCol(0) == Some((0, 0)))
    assertEquals(lm0.toLineCol(1), Some((1, 0)))

    val lm1 = LocationMap("012\n345\n678")
    assert(lm1.toLineCol(-1) == None)
    assert(lm1.toLineCol(0) == Some((0, 0)))
    assert(lm1.toLineCol(1) == Some((0, 1)))
    assert(lm1.toLineCol(2) == Some((0, 2)))
    assert(lm1.toLineCol(3) == Some((0, 3)))
    assert(lm1.toLineCol(4) == Some((1, 0)))
    assert(lm1.toLineCol(5) == Some((1, 1)))
    assert(lm1.toLineCol(6) == Some((1, 2)))
    assert(lm1.toLineCol(7) == Some((1, 3)))
    assert(lm1.toLineCol(8) == Some((2, 0)))
    assert(lm1.toLineCol(9) == Some((2, 1)))
    assert(lm1.toLineCol(10) == Some((2, 2)))
    assert(lm1.toLineCol(11) == Some((2, 3)))
  }

  property("we can reassemble input with getLine") {
    forAll { (str: String) =>
      val lm = LocationMap(str)

      val reconstruct = Iterator
        .iterate(0)(_ + 1)
        .map(lm.getLine _)
        .takeWhile(_.isDefined)
        .collect { case Some(l) => l }
        .mkString("\n")

      assertEquals(reconstruct, str)
    }
  }

  property("toLineCol is defined for all valid offsets, and getLine isDefined consistently") {

    forAll { (s: String, offset: Int) =>
      val lm = LocationMap(s)

      def test(offset: Int) =
        lm.toLineCol(offset) match {
          case None =>
            assert(offset < 0 || offset >= s.length)
          case Some((row, col)) =>
            lm.getLine(row) match {
              case None => fail(s"offset = $offset, s = $s")
              case Some(line) =>
                assert(line.length >= col)
                if (line.length == col) assert((offset == s.length) || s(offset) == '\n')
                else assert(line(col) == s(offset))
            }
        }

      test(offset)
      if (s.nonEmpty) test(math.abs(offset % s.length))
    }
  }

  property("if a string is not empty, 0 offset is (0, 0)") {
    forAll { (s: String) =>
      LocationMap(s).toLineCol(0) match {
        case Some(r) => assertEquals(r, ((0, 0)))
        case None => fail("could not get the first item")
      }
    }
  }

  property("slow toLineCol matches") {

    def slow(str: String, offset: Int): Option[(Int, Int)] = {
      val split = str.split("\n", -1)
      def lineCol(off: Int, row: Int): (Int, Int) =
        if (row == split.length) (row, 0)
        else {
          val r = split(row)
          val extraNewLine =
            if (row < (split.length - 1)) 1 else 0 // all but the last have an extra newline
          val chars = r.length + extraNewLine

          if (off >= chars) lineCol(off - chars, row + 1)
          else (row, off)
        }

      if (offset < 0 || offset > str.length) None
      else if (offset == str.length) Some {
        if (offset == 0) (0, 0)
        else {
          val (l, c) = lineCol(offset - 1, 0)
          if (str.last == '\n') (l + 1, 0)
          else (l, c + 1)
        }
      }
      else Some(lineCol(offset, 0))
    }

    assert(slow("\n", 0) == Some((0, 0)))
    assert(LocationMap("\n").toLineCol(0) == Some((0, 0)))

    assertEquals(slow("\n", 1), Some((1, 0)))
    assertEquals(LocationMap("\n").toLineCol(1), Some((1, 0)))

    assert(slow(" \n", 1) == Some((0, 1)))
    assert(LocationMap(" \n").toLineCol(1) == Some((0, 1)))

    assertEquals(slow(" \n", 2), Some((1, 0)))
    assertEquals(LocationMap(" \n").toLineCol(2), Some((1, 0)))

    assert(slow(" \n ", 1) == Some((0, 1)))
    assert(LocationMap(" \n ").toLineCol(1) == Some((0, 1)))

    assert(slow("\n ", 1) == Some((1, 0)))
    assert(LocationMap("\n ").toLineCol(1) == Some((1, 0)))

    forAll { (str: String, offset: Int) =>
      val lm = LocationMap(str)
      assertEquals(lm.toLineCol(offset), slow(str, offset))
      (0 to str.length).foreach { o =>
        assertEquals(lm.toLineCol(o), slow(str, o))
      }
    }
  }

  property("if x > y && toLineCol(x).isDefined, then toLineCol(x) > toLineCol(y)") {
    forAll { (s: String, x: Int, y: Int) =>
      val lm = LocationMap(s)
      val lcx = lm.toLineCol(x)
      val lcy = lm.toLineCol(y)

      if (x > y && y >= 0 && lcx.isDefined) {
        (lcx, lcy) match {
          case (Some((lx, cx)), Some((ly, cy))) =>
            assert(lx > ly || ((lx == ly) && (cx > cy)))
          case other =>
            fail(other.toString)
        }
      }
    }
  }

  property("toLineCol toOffset round trips") {
    forAll { (s: String, offset: Int) =>
      val offsets = (-s.length to 2 * s.length).toSet + offset

      val lm = LocationMap(s)
      offsets.foreach { o =>
        lm.toLineCol(o) match {
          case Some((l, c)) =>
            assertEquals(lm.toOffset(l, c), Some(o))
          case None =>
            assert(o < 0 || o > s.length)
        }
      }
    }
  }

  property("lineCount and getLine are consistent") {
    forAll { (s: String) =>
      val lm = LocationMap(s)
      assert(s.endsWith(lm.getLine(lm.lineCount - 1).get))
    }
  }

  property("toLineCol and toCaret are consistent") {
    forAll { (s: String, other: Int) =>
      val lm = LocationMap(s)
      (0 to s.length).foreach { offset =>
        val c = lm.toCaretUnsafe(offset)
        val oc = lm.toCaret(offset)
        val lc = lm.toLineCol(offset)

        assertEquals(oc, Some(c))
        assertEquals(lc, oc.map { case c  => (c.line, c.col) })
      }

      if (other < 0 || s.length < other) {
        assert(scala.util.Try(lm.toCaretUnsafe(other)).isFailure)
        assertEquals(lm.toCaret(other), None)
        assertEquals(lm.toLineCol(other), None)
      }
    }
  }

  property("Caret ordering matches offset ordering") {
    forAll { (s: String, o1: Int, o2: Int) =>
      val lm = LocationMap(s)
      val c1 = lm.toCaret(o1)
      val c2 = lm.toCaret(o2)

      if (c1.isDefined && c2.isDefined) {
        assertEquals(Ordering[Option[Caret]].compare(c1, c2), Integer.compare(o1, o2))
      }
    }
  }
}
