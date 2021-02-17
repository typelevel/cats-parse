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

/* Based on https://github.com/sirthias/parboiled2/blob/v2.2.1/examples/src/main/scala/org/parboiled2/examples/JsonParser.scala */
package cats.parse.bench.parboiled2

import scala.annotation.switch
import org.parboiled2._
import org.typelevel.jawn.ast._

/** This is a feature-complete JSON parser implementation that almost directly
  * models the JSON grammar presented at http://www.json.org as a parboiled2 PEG parser.
  */
class JsonParser(val input: ParserInput) extends Parser with StringBuilding {
  import CharPredicate.{Digit, Digit19, HexDigit}
  import JsonParser._

  // the root rule
  def Json = rule(WhiteSpace ~ Value ~ EOI)

  def JsonObject: Rule1[JObject] =
    rule {
      ws('{') ~ zeroOrMore(Pair).separatedBy(ws(',')) ~ ws('}') ~> (
        (fields: Seq[(String, JValue)]) => JObject.fromSeq(fields)
      )
    }

  def Pair = rule(JsonStringUnwrapped ~ ws(':') ~ Value ~> ((_, _)))

  def Value: Rule1[JValue] =
    rule {
      // as an optimization of the equivalent rule:
      //   JsonString | JsonNumber | JsonObject | JsonArray | JsonTrue | JsonFalse | JsonNull
      // we make use of the fact that one-char lookahead is enough to discriminate the cases
      run {
        (cursorChar: @switch) match {
          case '"' => JsonString
          case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '-' => JsonNumber
          case '{' => JsonObject
          case '[' => JsonArray
          case 't' => JsonTrue
          case 'f' => JsonFalse
          case 'n' => JsonNull
          case _ => MISMATCH
        }
      }
    }

  def JsonString = rule(JsonStringUnwrapped ~> (JString(_)))

  def JsonStringUnwrapped = rule('"' ~ clearSB() ~ Characters ~ ws('"') ~ push(sb.toString))

  def JsonNumber = rule(capture(Integer ~ optional(Frac) ~ optional(Exp)) ~> (JNum(_)) ~ WhiteSpace)

  def JsonArray = rule(
    ws('[') ~ zeroOrMore(Value).separatedBy(ws(',')) ~ ws(']') ~> (JArray.fromSeq(_))
  )

  def Characters = rule(zeroOrMore(NormalChar | '\\' ~ EscapedChar))

  def NormalChar = rule(!QuoteBackslash ~ ANY ~ appendSB())

  def EscapedChar =
    rule(
      QuoteSlashBackSlash ~ appendSB()
        | 'b' ~ appendSB('\b')
        | 'f' ~ appendSB('\f')
        | 'n' ~ appendSB('\n')
        | 'r' ~ appendSB('\r')
        | 't' ~ appendSB('\t')
        | Unicode ~> { code => sb.append(code.asInstanceOf[Char]); () }
    )

  def Unicode = rule(
    'u' ~ capture(HexDigit ~ HexDigit ~ HexDigit ~ HexDigit) ~> (java.lang.Integer.parseInt(_, 16))
  )

  def Integer = rule(optional('-') ~ (Digit19 ~ Digits | Digit))

  def Digits = rule(oneOrMore(Digit))

  def Frac = rule("." ~ Digits)

  def Exp = rule(ignoreCase('e') ~ optional(anyOf("+-")) ~ Digits)

  def JsonTrue = rule("true" ~ WhiteSpace ~ push(JTrue))

  def JsonFalse = rule("false" ~ WhiteSpace ~ push(JFalse))

  def JsonNull = rule("null" ~ WhiteSpace ~ push(JNull))

  def WhiteSpace = rule(zeroOrMore(WhiteSpaceChar))

  def ws(c: Char) = rule(c ~ WhiteSpace)
}

object JsonParser {
  val WhiteSpaceChar = CharPredicate(" \n\r\t\f")
  val QuoteBackslash = CharPredicate("\"\\")
  val QuoteSlashBackSlash = QuoteBackslash ++ "/"
}
