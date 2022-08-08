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

/** Parsers for common string literals
  */
object Strings {

  /** Parses a quoted json string */
  def jsonString: Parser[String] = Impl.JsonStringUtil.escapedString

  /** Parser a json string without quotes */
  def jsonUndelimedString: Parser0[String] =
    Impl.JsonStringUtil.undelimitedString1.orElse(Parser.pure(""))

  /** Escapes a string such that it would parse as itself with jsonUndelimitedString */
  def jsonEscape(str: String): String = Impl.JsonStringUtil.escape(str)

  private[this] object Impl {
    import cats.parse.{Parser => P}

    object JsonStringUtil {
      // Here are the rules for escaping in json
      val decodeTable: Map[Char, Char] =
        Map(
          ('\\', '\\'),
          ('/', '/'),
          ('\"', '\"'),
          ('b', 8.toChar), // backspace
          ('f', 12.toChar), // form-feed
          ('n', '\n'),
          ('r', '\r'),
          ('t', '\t')
        )

      private val encodeTable = decodeTable.iterator.map { case (v, k) => (k, s"\\$v") }.toMap

      private val nonPrintEscape: Array[String] =
        (0 until 32).map { c =>
          val strHex = c.toHexString
          val strPad = List.fill(4 - strHex.length)('0').mkString
          s"\\u$strPad$strHex"
        }.toArray

      val escapedToken: P[Char] = {
        def parseIntStr(p: P[String]): P[Char] =
          p.map(Integer.parseInt(_, 16).toChar)

        val escapes = P.charIn(decodeTable.keys.toSeq).map(decodeTable(_))

        val hex4 = P.charIn(('0' to '9') ++ ('a' to 'f') ++ ('A' to 'F')).repExactlyAs[String](4)
        val u4 = P.char('u') *> parseIntStr(hex4)

        // do the oneOf in a guess order of likelihood
        val after = P.oneOf(escapes :: u4 :: Nil)
        P.char('\\') *> after
      }

      val notEscape: P[Char] =
        P.charIn((0x20.toChar to 0x10ffff.toChar).toSet - '\\' - '"')

      /** String content without the delimiter
        */
      val undelimitedString1: P[String] =
        notEscape.orElse(escapedToken).repAs

      val escapedString: P[String] =
        P.char('\"') *> undelimitedString1.orElse(P.pure("")) <* P.char('\"')

      def escape(str: String): String = {
        val lowest = 0x20.toChar
        val highest = 0x10ffff.toChar
        str.flatMap { c =>
          encodeTable.get(c) match {
            case None =>
              if ((c < lowest) || (c > highest)) nonPrintEscape(c.toInt)
              else c.toString
            case Some(esc) => esc
          }
        }
      }
    }
  }
}
