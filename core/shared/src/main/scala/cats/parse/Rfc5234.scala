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

/** Parsers for the common rules of RFC5234. These rules are
  * referenced by several RFCs.
  *
  * @see [[https://tools.ietf.org/html/rfc5234]]
  */
object Rfc5234 {

  /** A-Z and a-z, without diacritics
    */
  val alpha: Parser1[Char] =
    Parser.charIn('A' to 'Z').orElse1(Parser.charIn('a' to 'z'))

  /** `0` or `1`
    */
  val bit: Parser1[Char] =
    Parser.charIn('0' to '1')

  /** any 7-bit US-ASCII character, excluding NUL
    */
  val char: Parser1[Char] =
    Parser.charIn(0x01.toChar to 0x7f.toChar)

  /** carriage return
    */
  val cr: Parser1[Unit] =
    Parser.char('\r')

  /** linefeed
    */
  val lf: Parser1[Unit] =
    Parser.char('\n')

  /** Internet standard newline */
  val crlf: Parser1[Unit] =
    Parser.string1("\r\n")

  /** controls */
  val ctl: Parser1[Char] =
    Parser.charIn(0x7f, (0x00.toChar to 0x1f.toChar): _*)

  /** `0` to `9`
    */
  val digit: Parser1[Char] =
    Numbers.digit

  /** double quote (`"`)
    */
  val dquote: Parser1[Unit] =
    Parser.char('"')

  /** hexadecimal digit, case insensitive
    */
  val hexdig: Parser1[Char] =
    digit.orElse1(Parser.ignoreCaseCharIn('A' to 'F'))

  /** horizontal tab
    */
  val htab: Parser1[Unit] =
    Parser.char('\t')

  /** space */
  val sp: Parser1[Unit] =
    Parser.char(' ')

  /** white space (space or horizontal tab) */
  val wsp: Parser1[Unit] =
    sp.orElse1(htab)

  /** linear white space.
    *
    * Use of this rule permits lines containing only white space that
    * are no longer legal in mail headers and have caused
    * interoperability problems in other contexts.
    *
    * Do not use when defining mail headers and use with caution in
    * other contexts.
    */
  val lwsp: Parser[Unit] =
    Parser.rep(wsp.orElse1(crlf *> wsp)).void

  /** 8 bits of data
    */
  val octet: Parser1[Char] =
    Parser.charIn(0x00.toChar to 0xff.toChar)

  /** visible (printing) characters
    */
  val vchar: Parser1[Char] =
    Parser.charIn(0x21.toChar to 0x7e.toChar)
}
