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

/** SemVer 2.0.0 Parser based on https://semver.org */
object SemVer {

  val dot: Parser1[Char] = Parser.charIn('.')
  val hyphen: Parser1[Char] = Parser.charIn('-')
  val plus: Parser1[Char] = Parser.charIn('+')

  val letter: Parser1[Char] = Parser.ignoreCaseCharIn('a' to 'z')

  val positiveDigit: Parser1[Char] = Numbers.nonZeroDigit

  val nonDigit: Parser1[Char] = letter orElse1 hyphen

  val identifierChar: Parser1[Char] = Numbers.digit orElse1 nonDigit

  val identifierChars: Parser1[String] = identifierChar.rep1.string

  val numericIdentifier: Parser1[String] = Numbers.nonNegativeIntString

  val alphanumericIdentifier: Parser1[String] = identifierChars

  val buildIdentifier: Parser1[String] = alphanumericIdentifier

  val preReleaseIdentifier: Parser1[String] = alphanumericIdentifier

  val dotSeparatedBuildIdentifiers: Parser1[String] = Parser.rep1Sep(buildIdentifier, 1, dot).string

  val build: Parser1[String] = dotSeparatedBuildIdentifiers

  val dotSeparatedPreReleaseIdentifiers: Parser1[String] =
    Parser.rep1Sep(preReleaseIdentifier, 1, dot).string

  val preRelease: Parser1[String] = dotSeparatedPreReleaseIdentifiers

  val patch: Parser1[String] = numericIdentifier
  val minor: Parser1[String] = numericIdentifier
  val major: Parser1[String] = numericIdentifier

  val core: Parser1[String] = (major ~ dot ~ minor ~ dot ~ patch).string

  val semver: Parser1[String] =
    ((core ~ hyphen ~ preRelease ~ plus ~ build).backtrack orElse1
      (core ~ plus ~ build).backtrack orElse1
      (core ~ hyphen ~ preRelease).backtrack orElse1
      core).string
}
