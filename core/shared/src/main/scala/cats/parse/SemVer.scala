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

  val dot: Parser1[Char] = Parser0.charIn('.')
  val hyphen: Parser1[Char] = Parser0.charIn('-')
  val plus: Parser1[Char] = Parser0.charIn('+')

  val letter: Parser1[Char] = Parser0.ignoreCaseCharIn('a' to 'z')

  def positiveDigit: Parser1[Char] = Numbers.nonZeroDigit

  val nonDigit: Parser1[Char] = letter.orElse1(hyphen)

  val identifierChar: Parser1[Char] = Numbers.digit.orElse1(nonDigit)

  val identifierChars: Parser1[String] = identifierChar.rep1.string

  def numericIdentifier: Parser1[String] = Numbers.nonNegativeIntString

  val alphanumericIdentifier: Parser1[String] = identifierChars

  val buildIdentifier: Parser1[String] = alphanumericIdentifier

  val preReleaseIdentifier: Parser1[String] = alphanumericIdentifier

  val dotSeparatedBuildIdentifiers: Parser1[String] = Parser0.rep1Sep(buildIdentifier, 1, dot).string

  val build: Parser1[String] = dotSeparatedBuildIdentifiers

  val dotSeparatedPreReleaseIdentifiers: Parser1[String] =
    Parser0.rep1Sep(preReleaseIdentifier, 1, dot).string

  val preRelease: Parser1[String] = dotSeparatedPreReleaseIdentifiers

  val patch: Parser1[String] = numericIdentifier
  val minor: Parser1[String] = numericIdentifier
  val major: Parser1[String] = numericIdentifier

  val core: Parser1[Core] = (major, dot *> minor, dot *> patch).mapN(Core)
  val coreString: Parser1[String] = core.string

  val semver: Parser1[SemVer] =
    (core ~ (hyphen *> preRelease).? ~ (plus *> build).?).map { case ((c, p), b) =>
      SemVer(c, p, b)
    }
  val semverString: Parser1[String] = semver.string
}
