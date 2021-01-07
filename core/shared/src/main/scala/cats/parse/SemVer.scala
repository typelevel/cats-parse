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

import cats.implicits._

/** SemVer 2.0.0 Parser based on https://semver.org */
object SemVer {

  case class Core(major: String, minor: String, patch: String)

  case class SemVer(core: Core, preRelease: Option[String], buildMetadata: Option[String])

  val dot: Parser[Char] = Parser.charIn('.')
  val hyphen: Parser[Char] = Parser.charIn('-')
  val plus: Parser[Char] = Parser.charIn('+')

  val letter: Parser[Char] = Parser.ignoreCaseCharIn('a' to 'z')

  def positiveDigit: Parser[Char] = Numbers.nonZeroDigit

  val nonDigit: Parser[Char] = letter | hyphen

  val identifierChar: Parser[Char] = Numbers.digit | nonDigit

  val identifierChars: Parser[String] = identifierChar.rep.string

  def numericIdentifier: Parser[String] = Numbers.nonNegativeIntString

  val alphanumericIdentifier: Parser[String] = identifierChars

  val buildIdentifier: Parser[String] = alphanumericIdentifier

  val preReleaseIdentifier: Parser[String] = alphanumericIdentifier

  val dotSeparatedBuildIdentifiers: Parser[String] = Parser.repSep(buildIdentifier, dot).string

  val build: Parser[String] = dotSeparatedBuildIdentifiers

  val dotSeparatedPreReleaseIdentifiers: Parser[String] =
    Parser.repSep(preReleaseIdentifier, dot).string

  val preRelease: Parser[String] = dotSeparatedPreReleaseIdentifiers

  val patch: Parser[String] = numericIdentifier
  val minor: Parser[String] = numericIdentifier
  val major: Parser[String] = numericIdentifier

  val core: Parser[Core] = (major, dot *> minor, dot *> patch).mapN(Core.apply)
  val coreString: Parser[String] = core.string

  val semver: Parser[SemVer] =
    (core ~ (hyphen *> preRelease).? ~ (plus *> build).?).map { case ((c, p), b) =>
      SemVer(c, p, b)
    }
  val semverString: Parser[String] = semver.string
}
