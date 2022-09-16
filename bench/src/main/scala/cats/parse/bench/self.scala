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

package cats.parse.bench.self

import cats.parse.{Parser0 => P0, Parser => P, Numbers, strings}
import org.typelevel.jawn.ast._

/* Based on https://github.com/johnynek/bosatsu/blob/7f4b75356c207b0e0eb2ab7d39f646e04b4004ca/core/src/main/scala/org/bykn/bosatsu/Json.scala */
object Json {
  private[this] val whitespace: P[Unit] = P.charIn(" \t\r\n").void
  private[this] val whitespaces0: P0[Unit] = whitespace.rep0.void

  /** This doesn't have to be super fast (but is fairly fast) since we use it in places where speed
    * won't matter: feeding it into a program that will convert it to bosatsu structured data
    */
  val parser: P[JValue] = P.recursive[JValue] { recurse =>
    val pnull = P.string("null").as(JNull)
    val bool = P.string("true").as(JBool.True).orElse(P.string("false").as(JBool.False))
    val justStr = strings.Json.delimited.parser
    val str = justStr.map(JString(_))
    val num = Numbers.jsonNumber.map(JNum(_))

    val listSep: P[Unit] =
      P.char(',').soft.surroundedBy(whitespaces0).void

    def rep0[A](pa: P[A]): P0[List[A]] =
      pa.repSep0(listSep).surroundedBy(whitespaces0)

    val list = rep0(recurse).with1
      .between(P.char('['), P.char(']'))
      .map { vs => JArray.fromSeq(vs) }

    val kv: P[(String, JValue)] =
      justStr ~ (P.char(':').surroundedBy(whitespaces0) *> recurse)

    val obj = rep0(kv).with1
      .between(P.char('{'), P.char('}'))
      .map { vs => JObject.fromSeq(vs) }

    P.oneOf(str :: num :: list :: obj :: bool :: pnull :: Nil)
  }

  // any whitespace followed by json followed by whitespace followed by end
  val parserFile: P[JValue] = parser.between(whitespaces0, whitespaces0 ~ P.end)
}
