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

/* Based on https://github.com/J-mie6/Parsley/blob/e5947782ee007ee9e5fc37eb9a668055e93c7f62/src/parsley/Benchmark.scala */
package cats.parse.bench.parsley

import org.http4s.parsley._
import org.http4s.parsley.Parsley._
import org.http4s.parsley.Combinator._
import org.typelevel.jawn.ast._

object ParsleyJson
{

    def json: Parsley[JValue] =
    {
        val jsontoks = LanguageDef("", "", "", false, NotRequired, NotRequired, NotRequired, NotRequired, Set.empty, Set.empty, true, Predicate(Char.isWhitespace))
        val tok = new TokenParser(jsontoks)
        lazy val obj: Parsley[JValue] = tok.braces(tok.commaSep(+(tok.stringLiteral <~> tok.colon *> value)).map(pairs => JObject.fromSeq(pairs)))
        lazy val array: Parsley[JValue] = tok.brackets(tok.commaSep(value)).map(list => JArray.fromSeq(list))
        lazy val value: Parsley[JValue] =
            (tok.stringLiteral.map(JString.apply)
         <|> tok.symbol("true") *> Parsley.pure(JTrue)
         <|> tok.symbol("false") *> Parsley.pure(JFalse)
         <|> tok.symbol("null") *> Parsley.pure(JNull)
         <|> array
         <|> attempt(tok.float).map(JNum.apply)
         <|> tok.integer.map(i => JNum.apply(i.toLong))
         <|> obj)

        tok.whiteSpace *> (obj <|> array) <* eof
    }
}
