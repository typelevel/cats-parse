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

/* Based on http://www.lihaoyi.com/fastparse/#Json */
package cats.parse.bench.fastparse

import org.typelevel.jawn.ast._
import _root_.fastparse._, NoWhitespace._

object JsonParse {
  def stringChars(c: Char) = c != '\"' && c != '\\'

  def space(implicit ctx: P[_]) = P(CharsWhileIn(" \r\n", 0))
  def digits(implicit ctx: P[_]) = P(CharsWhileIn("0-9"))
  def exponent(implicit ctx: P[_]) = P(CharIn("eE") ~ CharIn("+\\-").? ~ digits)
  def fractional(implicit ctx: P[_]) = P("." ~ digits)
  def integral(implicit ctx: P[_]) = P("0" | CharIn("1-9") ~ digits.?)

  def number(implicit ctx: P[_]) =
    P(CharIn("+\\-").? ~ integral ~ fractional.? ~ exponent.?).!.map(x => JNum(x.toDouble))

  def `null`(implicit ctx: P[_]) = P("null").map(_ => JNull)
  def `false`(implicit ctx: P[_]) = P("false").map(_ => JFalse)
  def `true`(implicit ctx: P[_]) = P("true").map(_ => JTrue)

  def hexDigit(implicit ctx: P[_]) = P(CharIn("0-9a-fA-F"))
  def unicodeEscape(implicit ctx: P[_]) = P("u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit)
  def escape(implicit ctx: P[_]) = P("\\" ~ (CharIn("\"/\\\\bfnrt") | unicodeEscape))

  def strChars(implicit ctx: P[_]) = P(CharsWhile(stringChars))
  def string(implicit ctx: P[_]) =
    P(space ~ "\"" ~/ (strChars | escape).rep.! ~ "\"").map(JString.apply)

  def array(implicit ctx: P[_]) =
    P("[" ~/ jsonExpr.rep(sep = ","./) ~ space ~ "]").map(JArray.fromSeq)

  def pair(implicit ctx: P[_]) = P(string.map(_.asString) ~/ ":" ~/ jsonExpr)

  def obj(implicit ctx: P[_]) =
    P("{" ~/ pair.rep(sep = ","./) ~ space ~ "}").map(JObject.fromSeq)

  def jsonExpr(implicit ctx: P[_]): P[JValue] = P(
    space ~ (obj | array | string | `true` | `false` | `null` | number) ~ space
  )
}
