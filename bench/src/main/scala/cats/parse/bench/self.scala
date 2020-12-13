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

package cats.parse.bench.self

import cats.implicits._
import cats.parse.{Parser0 => P, Parser1 => P1, Numbers}
import org.typelevel.jawn.ast._

/* Based on https://github.com/johnynek/bosatsu/blob/7f4b75356c207b0e0eb2ab7d39f646e04b4004ca/core/src/main/scala/org/bykn/bosatsu/Json.scala */
object Json {
  private[this] val whitespace: P1[Unit] = P.charIn(" \t\r\n").void
  private[this] val whitespaces0: P[Unit] = whitespace.rep.void

  /** This doesn't have to be super fast (but is fairly fast) since we use it in places
    * where speed won't matter: feeding it into a program that will convert it to bosatsu
    * structured data
    */
  val parser: P1[JValue] = {
    val recurse = P.defer1(parser)
    val pnull = P.string1("null").as(JNull)
    val bool = P.string1("true").as(JBool.True).orElse1(P.string1("false").as(JBool.False))
    val justStr = JsonStringUtil.escapedString('"')
    val str = justStr.map(JString(_))
    val num = Numbers.jsonNumber.map(JNum(_))

    val listSep: P1[Unit] =
      P.char(',').surroundedBy(whitespaces0).void

    def rep[A](pa: P1[A]): P[List[A]] =
      P.repSep(pa, min = 0, sep = listSep).surroundedBy(whitespaces0)

    val list = rep(recurse).with1
      .between(P.char('['), P.char(']'))
      .map { vs => JArray.fromSeq(vs) }

    val kv: P1[(String, JValue)] =
      justStr ~ (P.char(':').surroundedBy(whitespaces0) *> recurse)

    val obj = rep(kv).with1
      .between(P.char('{'), P.char('}'))
      .map { vs => JObject.fromSeq(vs) }

    P.oneOf1(str :: num :: list :: obj :: bool :: pnull :: Nil)
  }

  // any whitespace followed by json followed by whitespace followed by end
  val parserFile: P1[JValue] = parser.between(whitespaces0, whitespaces0 ~ P.end)
}

object JsonStringUtil extends GenericStringUtil {
  // Here are the rules for escaping in json
  lazy val decodeTable: Map[Char, Char] =
    Map(
      ('\\', '\\'),
      ('\'', '\''),
      ('\"', '\"'),
      ('b', 8.toChar), // backspace
      ('f', 12.toChar), // form-feed
      ('n', '\n'),
      ('r', '\r'),
      ('t', '\t')
    )
}

abstract class GenericStringUtil {
  protected def decodeTable: Map[Char, Char]

  private val encodeTable = decodeTable.iterator.map { case (v, k) => (k, s"\\$v") }.toMap

  private val nonPrintEscape: Array[String] =
    (0 until 32).map { c =>
      val strHex = c.toHexString
      val strPad = List.fill(4 - strHex.length)('0').mkString
      s"\\u$strPad$strHex"
    }.toArray

  val escapedToken: P1[Unit] = {
    val escapes = P.charIn(decodeTable.keys.toSeq)

    val oct = P.charIn('0' to '7')
    val octP = P.char('o') ~ oct ~ oct

    val hex = P.charIn(('0' to '9') ++ ('a' to 'f') ++ ('A' to 'F'))
    val hex2 = hex ~ hex
    val hexP = P.char('x') ~ hex2

    val hex4 = hex2 ~ hex2
    val u4 = P.char('u') ~ hex4
    val hex8 = hex4 ~ hex4
    val u8 = P.char('U') ~ hex8

    val after = P.oneOf1[Any](escapes :: octP :: hexP :: u4 :: u8 :: Nil)
    (P.char('\\') ~ after).void
  }

  /** String content without the delimiter
    */
  def undelimitedString1(endP: P1[Unit]): P1[String] =
    escapedToken.backtrack
      .orElse1((!endP).with1 ~ P.anyChar)
      .rep1
      .string
      .flatMap { str =>
        unescape(str) match {
          case Right(str1) => P.pure(str1)
          case Left(_) => P.fail
        }
      }

  private val simpleString: P[String] =
    P.charsWhile(c => c >= ' ' && c != '"' && c != '\\')

  def escapedString(q: Char): P1[String] = {
    val end: P1[Unit] = P.char(q)
    end *> ((simpleString <* end).backtrack
      .orElse(undelimitedString1(end) <* end))
  }

  def escape(quoteChar: Char, str: String): String = {
    // We can ignore escaping the opposite character used for the string
    // x isn't escaped anyway and is kind of a hack here
    val ignoreEscape = if (quoteChar == '\'') '"' else if (quoteChar == '"') '\'' else 'x'
    str.flatMap { c =>
      if (c == ignoreEscape) c.toString
      else
        encodeTable.get(c) match {
          case None =>
            if (c < ' ') nonPrintEscape(c.toInt)
            else c.toString
          case Some(esc) => esc
        }
    }
  }

  def unescape(str: String): Either[Int, String] = {
    val sb = new java.lang.StringBuilder
    def decodeNum(idx: Int, size: Int, base: Int): Int = {
      val end = idx + size
      if (end <= str.length) {
        val intStr = str.substring(idx, end)
        val asInt =
          try Integer.parseInt(intStr, base)
          catch { case _: NumberFormatException => ~idx }
        sb.append(asInt.toChar)
        end
      } else ~(str.length)
    }
    @annotation.tailrec
    def loop(idx: Int): Int =
      if (idx >= str.length) {
        // done
        idx
      } else if (idx < 0) {
        // error from decodeNum
        idx
      } else {
        val c0 = str.charAt(idx)
        if (c0 != '\\') {
          sb.append(c0)
          loop(idx + 1)
        } else {
          // str(idx) == \
          val nextIdx = idx + 1
          if (nextIdx >= str.length) {
            // error we expect there to be a character after \
            ~idx
          } else {
            val c = str.charAt(nextIdx)
            decodeTable.get(c) match {
              case Some(d) =>
                sb.append(d)
                loop(idx + 2)
              case None =>
                c match {
                  case 'o' => loop(decodeNum(idx + 2, 2, 8))
                  case 'x' => loop(decodeNum(idx + 2, 2, 16))
                  case 'u' => loop(decodeNum(idx + 2, 4, 16))
                  case 'U' => loop(decodeNum(idx + 2, 8, 16))
                  case other =>
                    // \c is interpretted as just \c, if the character isn't escaped
                    sb.append('\\')
                    sb.append(other)
                    loop(idx + 2)
                }
            }
          }
        }
      }

    val res = loop(0)
    if (res < 0) Left(~res)
    else Right(sb.toString)
  }
}
