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

/* Based on http://www.lihaoyi.com/fastparse/#Json */
package cats.parse.bench.fastparse

import org.typelevel.jawn.ast._
import _root_.fastparse._, NoWhitespace._

object JsonParse {
  def stringChars(c: Char) = c != '\"' && c != '\\'

  def space[_: P] = P(CharsWhileIn(" \r\n", 0))
  def digits[_: P] = P(CharsWhileIn("0-9"))
  def exponent[_: P] = P(CharIn("eE") ~ CharIn("+\\-").? ~ digits)
  def fractional[_: P] = P("." ~ digits)
  def integral[_: P] = P("0" | CharIn("1-9") ~ digits.?)

  def number[_: P] =
    P(CharIn("+\\-").? ~ integral ~ fractional.? ~ exponent.?).!.map(x => JNum(x.toDouble))

  def `null`[_: P] = P("null").map(_ => JNull)
  def `false`[_: P] = P("false").map(_ => JFalse)
  def `true`[_: P] = P("true").map(_ => JTrue)

  def hexDigit[_: P] = P(CharIn("0-9a-fA-F"))
  def unicodeEscape[_: P] = P("u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit)
  def escape[_: P] = P("\\" ~ (CharIn("\"/\\\\bfnrt") | unicodeEscape))

  def strChars[_: P] = P(CharsWhile(stringChars))
  def string[_: P] =
    P(space ~ "\"" ~/ (strChars | escape).rep.! ~ "\"").map(JString.apply)

  def array[_: P] =
    P("[" ~/ jsonExpr.rep(sep = ","./) ~ space ~ "]").map(JArray.fromSeq)

  def pair[_: P] = P(string.map(_.asString) ~/ ":" ~/ jsonExpr)

  def obj[_: P] =
    P("{" ~/ pair.rep(sep = ","./) ~ space ~ "}").map(JObject.fromSeq)

  def jsonExpr[_: P]: P[JValue] = P(
    space ~ (obj | array | string | `true` | `false` | `null` | number) ~ space
  )
}
