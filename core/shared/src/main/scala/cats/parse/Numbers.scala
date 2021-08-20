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

object Numbers {

  /** a single base 10 digit
    */
  val digit: Parser[Char] =
    Parser.charIn('0' to '9')

  /** zero or more digit chars
    */
  val digits0: Parser0[String] = digit.rep0.string

  /** one or more digit chars
    */
  val digits: Parser[String] = digit.rep.string

  /** a single base 10 digit excluding 0
    */
  val nonZeroDigit: Parser[Char] =
    Parser.charIn('1' to '9')

  /** A String of either 1 '0' or 1 non-zero digit followed by zero or more digits
    */
  val nonNegativeIntString: Parser[String] =
    (nonZeroDigit ~ digits0).void
      .orElse(Parser.char('0'))
      .string

  /** A nonNegativeIntString possibly preceded by '-'
    */
  val signedIntString: Parser[String] =
    (Parser.char('-').?.with1 ~ nonNegativeIntString).string

  /** map a signedIntString into a BigInt
    */
  val bigInt: Parser[BigInt] =
    signedIntString.map(BigInt(_))

  /** A string matching the json specification for numbers. from:
    * https://tools.ietf.org/html/rfc4627
    */
  val jsonNumber: Parser[String] = {
    /*
     *     number = [ minus ] int [ frac ] [ exp ]
     *     decimal-point = %x2E       ; .
     *     digit1-9 = %x31-39         ; 1-9
     *     e = %x65 / %x45            ; e E
     *     exp = e [ minus / plus ] 1*DIGIT
     *     frac = decimal-point 1*DIGIT
     *     int = zero / ( digit1-9 *DIGIT )
     *     minus = %x2D               ; -
     *     plus = %x2B                ; +
     *     zero = %x30                ; 0
     */
    val frac: Parser[Any] = Parser.char('.') ~ digits
    val exp: Parser[Unit] = (Parser.charIn("eE") ~ Parser.charIn("+-").? ~ digits).void

    (signedIntString ~ frac.? ~ exp.?).string
  }

}
