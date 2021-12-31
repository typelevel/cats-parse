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
    def parser(args: Any*): Parser0[Any] = macro StringInterpolationMacros.parser
  }

}

private class StringInterpolationMacros(val c: whitebox.Context) {
  import c.universe._

  private def lit(s: String) = q"_root_.cats.parse.Parser.string($s)"

  final def parser(args: c.Expr[Any]*) =
    c.prefix.tree match {
      case Apply(_, Apply(_, parts) :: Nil) =>
        // This has exactly 1 more than args (before, between and after
        val stringParts = parts.collect { case Literal(Constant(s: String)) => s }
        val stringHead = stringParts.head

        // If there is at least one Parser, we can return a Parser, else Parser0
        val returnParser = stringParts.exists(_.nonEmpty)
        val unitParser = q"_root_.cats.parse.Parser.unit"
        def all(xs: List[Tree]): Tree =
          xs match {
            case Nil => sys.error("invariant violation: we never call this with size 0")
            case h :: Nil => h
            case _ =>
              Apply(
                Select(q"_root_.cats.Semigroupal", TermName(s"tuple${args.length}")),
                xs
              )
          }

        def stringBefore(s: String, t: Tree): Tree =
          q"${lit(s)} *> $t"

        def stringAfter(t: Tree, s: String): Tree =
          q"$t.with1 <* ${lit(s)}"

        val argsTree = args.map(_.tree)

        if (args.isEmpty) {
          // there is a single string
          if (stringHead.nonEmpty) lit(stringHead) else unitParser
        } else if (stringHead.nonEmpty) {
          // we have an initial string, so we can definitely do a Parser
          val right: List[Tree] =
            argsTree.toList.zip(stringParts.tail).map { case (a, b) =>
              if (b.isEmpty) a else stringAfter(a, b)
            }

          stringBefore(stringHead, all(right))
        } else {
          val last = stringParts.last
          val suffix = all {
            stringParts.init.zip(argsTree).map { case (a, b) =>
              if (a.isEmpty) b else stringBefore(a, b)
            }
          }

          if (last.nonEmpty) {
            // we have an trailing string, so we can definitely do a Parser
            stringAfter(suffix, last)
          } else {
            // both the head and tail are empty strings
            // this is uglier, but should still be valid
            if (returnParser) q"$suffix.unsafeCastToParser"
            else suffix
          }
        }
      case _ => c.abort(c.enclosingPosition, "Must be called as string interpolator")
    }
}
