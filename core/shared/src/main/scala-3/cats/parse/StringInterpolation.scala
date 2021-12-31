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

  private def parserImpl(sc: Expr[StringContext], args: Expr[Seq[Any]])(using q: Quotes) = {
    import quotes.reflect._

    // TODO: this is awful, seems like there should be a scala function for this
    def tupleOf(lst: List[TypeTree]): TypeTree =
      lst.size match {
        case 0 => sys.error("invalid arg: don't call on empty lists")
        case 1 => lst.head
        case 2 => Applied(TypeTree.of[Tuple2], lst)
        case 3 => Applied(TypeTree.of[Tuple3], lst)
        case 4 => Applied(TypeTree.of[Tuple4], lst)
        case 5 => Applied(TypeTree.of[Tuple5], lst)
        case 6 => Applied(TypeTree.of[Tuple6], lst)
        case 7 => Applied(TypeTree.of[Tuple7], lst)
        case 8 => Applied(TypeTree.of[Tuple8], lst)
        case 9 => Applied(TypeTree.of[Tuple9], lst)
        case 10 => Applied(TypeTree.of[Tuple10], lst)
        case 11 => Applied(TypeTree.of[Tuple11], lst)
        case 12 => Applied(TypeTree.of[Tuple12], lst)
        case 13 => Applied(TypeTree.of[Tuple13], lst)
        case 14 => Applied(TypeTree.of[Tuple14], lst)
        case 15 => Applied(TypeTree.of[Tuple15], lst)
        case 16 => Applied(TypeTree.of[Tuple16], lst)
        case 17 => Applied(TypeTree.of[Tuple17], lst)
        case 18 => Applied(TypeTree.of[Tuple18], lst)
        case 19 => Applied(TypeTree.of[Tuple19], lst)
        case 20 => Applied(TypeTree.of[Tuple20], lst)
        case 21 => Applied(TypeTree.of[Tuple21], lst)
        case 22 => Applied(TypeTree.of[Tuple22], lst)
        case l =>
          throw new IllegalStateException(
            s"tuples only as large as 22 supported, found: $l"
          )
      }

    def lit(s: String): Term = {
      val stringExpr = Expr(s)
      '{ Parser.string($stringExpr) }.asTerm
    }

    def summonImplicits[F[_]: Type] = List(
      Expr.summon[cats.Semigroupal[F]],
      Expr.summon[cats.Invariant[F]]
    ).map(
      _.getOrElse(
        throw new Exception("Could not find cats implicit instance for Parser!")
      ).asTerm
    )

    // This has exactly 1 more than args (before, between and after)
    val stringParts = sc match {
      case '{ StringContext(${ Varargs(Exprs(parts)) }: _*) } => parts.toList
      case _ =>
        throw new IllegalStateException(
          "Unexpected: this method may have been called outside the context of a string interpolator"
        )
    }
    val stringHead = stringParts.head

    val argTerms: List[(Term, TypeTree, Boolean)] = args match {
      case Varargs(argExprs) =>
        argExprs.toList.map {
          case '{ $arg: Parser[t] } => (arg.asTerm, TypeTree.of[t], false)
          case '{ $arg: Parser0[t] } => (arg.asTerm, TypeTree.of[t], true)
          case o => throw new IllegalArgumentException(s"${o.asTerm.tpe} is not a Parser")
        }
      case other =>
        throw new IllegalStateException(
          "Unexpected: this method may have been called outside the context of a string interpolator"
        )
    }

    // If there is at least one Parser, we can return a Parser, else Parser0
    val returnParser = argTerms.exists(!_._3) || stringParts.exists(_.nonEmpty)

    val tupleType = argTerms.map(_._2)

    val unitParser = '{ cats.parse.Parser.unit }.asTerm
    def all(parsers: List[Term]): Term =
      parsers match {
        case Nil => sys.error("invariant violation: we never call on empty lists")
        case one :: Nil => one
        case _ =>
          // it's possible to call `etaExpand` on the expression below without implicit arguments
          // which will give us a `Block` we could theoretically inspect and summon the appropriate implicits for.
          // However, that is a complicated general solution and we can probably assume that SemiGroupal.tuple[n]
          // won't change anytime soon, so we can just hard-code the implicits necessary.

          // IE, Instead of building
          // `Semigroupal.tuple2[Parser, A, B](a, b)`
          // we build
          // a `Semigroupal.tuple2[Parser, A, B](a, b)(implicitly[Semigroupal[Parser]], implicitly[Invariant[Parser]])

          // NB: we need to find the LUB of all our parsers; it will be either `Parser0` or `Parser`.
          // but we need to get the types lined up for the tupler to work
          val usesParser0 = argTerms.map(_._3).contains(true)
          val catsImplicits = if (usesParser0) summonImplicits[Parser0] else summonImplicits[Parser]
          val parserType =
            (if (usesParser0) TypeTree.of[Parser0] else TypeTree.of[Parser]) :: tupleType

          Apply(
            Apply(
              TypeApply(
                Select.unique('{ _root_.cats.Semigroupal }.asTerm, s"tuple${parsers.length}"),
                parserType
              ),
              parsers
            ),
            catsImplicits
          )
      }

    def stringBefore(s: String, t: Term, tpe: TypeTree): Term =
      Apply(
        TypeApply(Select.unique(lit(s), "*>"), List(tpe)),
        List(t)
      )

    def stringAfter(t: Term, s: String): Term =
      Apply(
        TypeApply(Select.unique(Select.unique(t, "with1"), "<*"), List(TypeTree.of[Unit])),
        List(lit(s))
      )

    val term =
      if (argTerms.isEmpty) {
        // there is a single string
        if (stringHead.nonEmpty) lit(stringHead) else unitParser
      } else if (stringHead.nonEmpty) {
        // we have an initial string, so we can definitely do a Parser
        val right: List[Term] =
          argTerms.zip(stringParts.tail).map { case ((aterm, _, _), b) =>
            if (b.isEmpty) aterm else stringAfter(aterm, b)
          }

        stringBefore(stringHead, all(right), tupleOf(tupleType))
      } else {
        val last = stringParts.last
        val suffix = all {
          stringParts.init.zip(argTerms).map { case (a, (bterm, btpe, _)) =>
            if (a.isEmpty) bterm else stringBefore(a, bterm, btpe)
          }
        }

        if (last.nonEmpty) {
          // we have an trailing string, so we can definitely do a Parser
          stringAfter(suffix, last)
        } else {
          // both the head and tail are empty strings
          // this is uglier, but should still be valid
          if (returnParser) Select.unique(suffix, "unsafeCastToParser")
          else suffix
        }
      }
    term.asExpr
  }
}
