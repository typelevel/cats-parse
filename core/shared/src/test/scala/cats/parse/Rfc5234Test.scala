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

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

class Rfc5234Test extends munit.ScalaCheckSuite {
  val allChars: Set[Char] = Set(Char.MinValue to Char.MaxValue: _*)

  def singleCharProperties[A](name: String, rule: Parser[A], valid: Set[Char], f: Char => A) = {
    val genValid = Gen.oneOf(valid)
    val genInvalid = Gen.oneOf(allChars -- valid)
    property(s"${name} parses single valid char") {
      forAll(genValid) { c =>
        assertEquals(rule.parseAll(c.toString), Right(f(c)))
      }
    }
    property(s"${name} rejects single invalid char") {
      forAll(genInvalid) { c =>
        assert(rule.parseAll(c.toString).isLeft)
      }
    }
    property(s"${name} rejects all but single char") {
      forAll(Gen.stringOf(genValid).filter(_.size != 1)) { c =>
        assert(rule.parseAll(c.toString).isLeft)
      }
    }
  }

  def singleConstCharProperties(name: String, rule: Parser[Unit], valid: Char) =
    singleCharProperties(name, rule, Set(valid), _ => ())

  def singleMultiCharProperties(name: String, rule: Parser[Char], valid: Set[Char]) =
    singleCharProperties(name, rule, valid, identity)

  singleMultiCharProperties(
    "alpha",
    Rfc5234.alpha,
    Set((0x41.toChar to 0x5a.toChar) ++ (0x61.toChar to 0x7a.toChar): _*)
  )
  singleMultiCharProperties("bit", Rfc5234.bit, Set('0', '1'))
  singleMultiCharProperties("char", Rfc5234.char, Set(0x01.toChar to 0x7f.toChar: _*))
  singleConstCharProperties("cr", Rfc5234.cr, 0x0d.toChar)
  singleMultiCharProperties("ctl", Rfc5234.char, Set(0x01.toChar to 0x1f.toChar: _*) + 0x7f.toChar)
  singleMultiCharProperties("digit", Rfc5234.digit, Set(0x30.toChar to 0x39.toChar: _*))
  singleConstCharProperties("dquote", Rfc5234.dquote, 0x22.toChar)
  singleMultiCharProperties(
    "hexdig",
    Rfc5234.hexdig,
    Set('0' to '9': _*) ++ Set('A' to 'F': _*) ++ Set('a' to 'F': _*)
  )
  singleConstCharProperties("htab", Rfc5234.htab, 0x09.toChar)
  singleConstCharProperties("lf", Rfc5234.lf, 0x0a.toChar)
  singleMultiCharProperties("octet", Rfc5234.octet, Set(0x00.toChar to 0xff.toChar: _*))
  singleConstCharProperties("sp", Rfc5234.sp, 0x20.toChar)
  singleMultiCharProperties("vchar", Rfc5234.vchar, Set(0x21.toChar to 0x7e.toChar: _*))
  singleCharProperties("wsp", Rfc5234.wsp, Set(0x20.toChar, 0x09.toChar), _ => ())

  test("crlf accepts \r\n") {
    assertEquals(Rfc5234.crlf.parseAll("\r\n"), Right(()))
  }
  property("crlf rejects all but \r\n") {
    forAll { s: String =>
      assert(Rfc5234.crlf.parseAll(s).isLeft)
    }
  }

  property("lwsp accepts all linear white space") {
    val genWsp = Gen.oneOf(0x20.toChar, 0x09.toChar)
    val genLwsp = Gen
      .listOf(
        Gen.oneOf(
          genWsp.map(_.toString),
          genWsp.map("\r\n" + _)
        )
      )
      .map(_.mkString(""))
    forAll(genLwsp) { s: String =>
      assertEquals(Rfc5234.lwsp.parseAll(s), Right(()))
    }
  }
  property("lwsp rejects crlf unless followed by wsp") {
    val gen = Gen
      .option(Gen.oneOf(allChars - 0x20.toChar - 0x09.toChar))
      .map(opt => "\r\n" ++ opt.fold("")(_.toString))
    forAll(gen) { s: String =>
      assert(Rfc5234.lwsp.parseAll(s).isLeft)
    }
  }
}
