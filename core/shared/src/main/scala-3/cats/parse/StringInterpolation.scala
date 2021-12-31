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

import cats.MonadThrow

import scala.quoted._

object StringInterpolation {

  extension (inline sc: StringContext) {
    transparent inline final def parser(inline args: Any*): Any = ${ parserImpl('sc, 'args) }
  }

  def parserImpl(sc: Expr[StringContext], args: Expr[Seq[Any]])(using q: Quotes) = {
    import quotes.reflect._

    def litParser(s: String): Term = {
      val stringExpr = Expr(s)
      '{ Parser.string($stringExpr) }.asTerm
    }

    val stringParts = sc match {
      case '{ StringContext(${ Varargs(Exprs(parts)) }: _*) } => parts.toList
      case _ =>
        throw new IllegalStateException(
          "Unexpected: this method may have been called outside the context of a string interpolator"
        )
    }

    val argTerms = args match {
      case Varargs(argExprs) =>
        argExprs.toList.map {
          case '{ $arg: Parser[t] } => (arg.asTerm, TypeTree.of[t])
          case o => throw new IllegalArgumentException(s"${o.asTerm.tpe} is not a Parser")
        }
      case other =>
        throw new IllegalStateException(
          "Unexpected: this method may have been called outside the context of a string interpolator"
        )
    }

    val (matched, last) = stringParts.splitAt(argTerms.length)

    val parsers = matched.zip(argTerms).map { case (a, (b, t)) =>
      if (a.isEmpty) b
      else {
        Apply(
          TypeApply(Select.unique(litParser(a), "*>"), List(t)),
          List(b)
        )
      }
    }

    val trailing = last.filter(_.nonEmpty).map(litParser).headOption

    ((parsers, trailing) match {
      case (Nil, None) => throw new IllegalArgumentException("a non-empty string is required to create a Parser")
      case (Nil, Some(t)) => t
      case (x :: Nil, Some(t)) => Apply(
        TypeApply(Select.unique(x, "<*"), List(TypeTree.of[Unit])),
        List(t)
      )
      case (x :: Nil, None) => x
      case (xs, t) =>
        // it's possible to call `etaExpand` on the expression below without implicit arguments
        // which will give us a `Block` we could theoretically inspect and summon the appropriate implicits for.
        // However, that is a complicated general solution and we can probably assume that SemiGroupal.tuple[n]
        // won't change anytime soon, so we can just hard-code the implicits necessary.

        // IE, Instead of building
        // `Semigroupal.tuple2[Parser, A, B](a, b)`
        // we build
        // a `Semigroupal.tuple2[Parser, A, B](a, b)(implicitly[Semigroupal[Parser]], implicitly[Invariant[Parser]])

        val catsImplicits = List(
          Expr.summon[cats.Semigroupal[Parser]],
          Expr.summon[cats.Invariant[Parser]]
        ).map(
          _.getOrElse(
            throw new Exception("Could not find cats implicit instance for Parser!")
          ).asTerm
        )

        val tupled = Apply(
          Apply(
            TypeApply(
              Select.unique('{ _root_.cats.Semigroupal }.asTerm, s"tuple${parsers.length}"),
              TypeTree.of[Parser] :: argTerms.map(_._2)
            ),
            parsers
          ),
          catsImplicits
        )

        t.map(p => Apply(
          TypeApply(Select.unique(tupled, "<*"), List(TypeTree.of[Unit])),
          List(p)
        )).getOrElse(tupled)
    }).asExpr


  }

}
