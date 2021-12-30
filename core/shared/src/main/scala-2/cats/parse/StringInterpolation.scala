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

import scala.reflect.macros.whitebox

object StringInterpolation {

  implicit class Helper(val sc: StringContext) extends AnyVal {
    def parser(args: Any*): Parser[Any] = macro StringInterpolationMacros.parser
  }

}

class StringInterpolationMacros(val c: whitebox.Context) {
  import c.universe._

  private def lit(s: String) = q"_root_.cats.parse.Parser.string($s)"

  final def parser(args: c.Expr[Any]*) =
    c.prefix.tree match {
      case Apply(_, Apply(_, parts) :: Nil) =>
        val stringParts = parts.collect { case Literal(Constant(s: String)) => s }

        val (matched, last) = stringParts.splitAt(args.length)

        val parsers: List[Tree] = matched.zip(args).map { case (a, b) =>
          if (a.isEmpty) b.tree else q"${lit(a)} *> $b "
        } ++ last.filter(_.nonEmpty).map(lit)

        parsers match {
          case Nil =>
            throw new IllegalArgumentException("a non-empty string is required to create a Parser")
          case x :: Nil => x
          case xs =>
            Apply(
              Select(q"_root_.cats.Semigroupal", TermName(s"tuple${parsers.length}")),
              xs
            )
        }

      case _ => c.abort(c.enclosingPosition, "Must be called as string interpolator")
    }
}
