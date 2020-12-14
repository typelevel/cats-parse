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

import cats.{Eval, FunctorFilter, Monad, Defer, Alternative, FlatMap, Now, MonoidK, Order}
import cats.data.{AndThen, Chain, NonEmptyList}

import cats.implicits._
import scala.collection.mutable.ListBuffer

/** Parser0[A] attempts to extract an `A` value from the given input,
  * potentially moving its offset forward in the process.
  *
  * When calling `parse`, one of three outcomes occurs:
  *
  *   - Success: The parser consumes zero-or-more characters of input
  *     and successfully extracts a value. The input offset will be
  *     moved forward by the number of characters consumed.
  *
  *   - Epsilon failure: The parser fails to extract a value without
  *     consuming any characters of input. The input offset will not be
  *     changed.
  *
  *   - Arresting failure: The parser fails to extract a value but does
  *     consume one-or-more characters of input. The input offset will
  *     be moved forward by the number of characters consumed and all
  *     parsing will stop (unless a higher-level parser backtracks).
  *
  * Operations such as `x.orElse(y)` will only consider parser `y` if
  * `x` returns an epsilon failure; these methods cannot recover from
  * an arresting failure. Arresting failures can be "rewound" using
  * methods such as `x.backtrack` (which converts arresting failures
  * from `x` into epsilon failures), or `softProduct(x, y)` (which can
  * rewind successful parses by `x` that are followed by epsilon
  * failures for `y`).
  *
  * Rewinding tends to make error reporting more difficult and can lead
  * to exponential parser behavior it is not the default behavior.
  */
sealed abstract class Parser0[+A] {

  /** Attempt to parse an `A` value out of `str`.
    *
    * This method will either return a failure, or else the remaining
    * string and the parsed value.
    *
    * To require the entire input to be consumed, see `parseAll`.
    */
  final def parse(str: String): Either[Parser0.Error, (String, A)] = {
    val state = new Parser0.Impl.State(str)
    val result = parseMut(state)
    val err = state.error
    val offset = state.offset
    if (err eq null) Right((str.substring(offset), result))
    else
      Left(Parser0.Error(offset, Parser0.Expectation.unify(NonEmptyList.fromListUnsafe(err.toList))))
  }

  /** Attempt to parse all of the input `str` into an `A` value.
    *
    * This method will return a failure unless all of `str` is consumed
    * during parsing.
    *
    * `p.parseAll(s)` is equivalent to `(p <* Parser0.end).parse(s).map(_._2)`.
    */
  final def parseAll(str: String): Either[Parser0.Error, A] = {
    val state = new Parser0.Impl.State(str)
    val result = parseMut(state)
    val err = state.error
    val offset = state.offset
    if (err eq null) {
      if (offset == str.length) Right(result)
      else
        Left(
          Parser0.Error(
            offset,
            NonEmptyList(Parser0.Expectation.EndOfString(offset, str.length), Nil)
          )
        )
    } else
      Left(Parser0.Error(offset, Parser0.Expectation.unify(NonEmptyList.fromListUnsafe(err.toList))))
  }

  /** Convert epsilon failures into None values.
    *
    * Normally if a parser fails to consume any input it fails with an
    * epsilon failure. The `?` method converts these failures into
    * None values (and wraps other values in `Some(_)`).
    *
    * If the underlying parser failed with other errors, this parser
    * will still fail.
    */
  def ? : Parser0[Option[A]] =
    Parser0.oneOf0(Parser0.map0(this)(Some(_)) :: Parser0.Impl.optTail)

  /** Parse without capturing values.
    *
    * Calling `void` on a parser can be a significant optimization --
    * it allows the parser to avoid allocating results to return.
    *
    * Other methods like `as`, `*>`, and `<*` use `void` internally to
    * discard allocations, since they will ignore the original parsed
    * result.
    */
  def void: Parser0[Unit] =
    Parser0.void0(this)

  /** Return the string matched by this parser.
    *
    * When parsing an input string that the underlying parser matches,
    * this parser will return the matched substring instead of any
    * value that the underlying parser would have returned. It will
    * still match exactly the same inputs as the original parser.
    *
    * This method is very efficient: similarly to `void`, we can avoid
    * allocating results to return.
    */
  def string: Parser0[String] =
    Parser0.string0(this)

  /** If this parser fails to match, rewind the offset to the starting
    * point before moving on to other parser.
    *
    * This method converts arresting failures into epsilon failures,
    * which includes rewinding the offset to that used before parsing
    * began.
    *
    * This method will most often be used before calling methods such
    * as `orElse`, `~`, or `flatMap` which involve a subsequent parser
    * picking up where this one left off.
    */
  def backtrack: Parser0[A] =
    Parser0.backtrack0(this)

  /** Sequence another parser after this one, combining both results
    * into a tuple.
    *
    * This combinator returns a product of parsers. If this parser
    * successfully produces an `A` value, the other parser is run on
    * the remaining input to try to produce a `B` value.
    *
    * If either parser produces an error the result is an error.
    * Otherwise both extracted values are combined into a tuple.
    */
  def ~[B](that: Parser0[B]): Parser0[(A, B)] =
    Parser0.product0(this, that)

  /** If this parser fails to parse its input with an epsilon error,
    * try the given parser instead.
    *
    * If this parser fails with an arresting error, the next parser
    * won't be tried.
    *
    * Backtracking may be used on the left parser to allow the right
    * one to pick up after any error, resetting any state that was
    * modified by the left parser.
    */
  def orElse0[A1 >: A](that: Parser0[A1]): Parser0[A1] =
    Parser0.oneOf0(this :: that :: Nil)

  /** Transform parsed values using the given function.
    *
    * This parser will match the same inputs as the underlying parser,
    * using the given function `f` to transform the values the
    * underlying parser produces.
    *
    * If the underlying value is ignored (e.g. `map(_ => ...)`) calling
    * `void` before `map` will improve the efficiency of the parser.
    */
  def map[B](fn: A => B): Parser0[B] =
    Parser0.map0(this)(fn)

  /** Transform parsed values using the given function, or fail on None
    *
    * When the function return None, this parser fails
    * This is implemented with select, which makes it more efficient
    * than using flatMap
    */
  def mapFilter[B](fn: A => Option[B]): Parser0[B] = {
    val leftUnit = Left(())

    val first = map { a =>
      fn(a) match {
        case Some(b) => Right(b)
        case None => leftUnit
      }
    }
    Parser0.select0(first)(Parser0.Fail)
  }

  /** Transform parsed values using the given function, or fail when not defined
    *
    * When the function is not defined, this parser fails
    * This is implemented with select, which makes it more efficient
    * than using flatMap
    */
  def collect[B](fn: PartialFunction[A, B]): Parser0[B] =
    mapFilter(fn.lift)

  /** If the predicate is not true, fail
    * you may want .filter(fn).backtrack so if the filter fn
    * fails you can fall through in an oneOf0 or orElse
    *
    * Without the backtrack, a failure of the function will
    * be an arresting failure.
    */
  def filter(fn: A => Boolean): Parser0[A] = {
    val leftUnit = Left(())
    Parser0.select0(this.map { a =>
      if (fn(a)) Right(a)
      else leftUnit
    })(Parser0.Fail)
  }

  /** Dynamically construct the next parser based on the previously
    * parsed value.
    *
    * Using `flatMap` is very expensive. When possible, you should
    * prefer to use methods such as `~`, `*>`, or `<*` when possible,
    * since these are much more efficient.
    */
  def flatMap[B](fn: A => Parser0[B]): Parser0[B] =
    Parser0.flatMap0(this)(fn)

  /** Replaces parsed values with the given value.
    */
  def as[B](b: B): Parser0[B] =
    Parser0.as0(this, b)

  /** Wrap this parser in a helper class, enabling better composition
    * with `Parser` values.
    *
    * For example, with `p: Parser0[Int]` and `p1: Parser0[Double]`:
    *
    *     val a1: Parser0[(Int, Double)]  = p ~ p1
    *     val a2: Parser[(Int, Double)] = p.with1 ~ p1
    *
    *     val b1: Parser0[Double]  = p *> p1
    *     val b2: Parser[Double] = p.with1 *> p1
    *
    *     val c1: Parser0[Int]  = p <* p1
    *     val c2: Parser[Int] = p.with1 <* p1
    *
    * Without using `with1`, these methods will return `Parser0` values
    * since they are not known to return `Parser` values instead.
    */
  def with1: Parser0.With1[A] =
    new Parser0.With1(this)

  /** Wrap this parser in a helper class, to enable backtracking during
    * composition.
    *
    * This wrapper changes the behavior of `~`, `<*` and `*>`. Normally
    * no backtracking occurs. Using `soft` on the left-hand side will
    * enable backtracking if the right-hand side returns an epsilon
    * failure (but not in any other case).
    *
    * For example, `(x ~ y)` will never backtrack. But with `(x.soft ~
    * y)`, if `x` parses successfully, and `y` returns an epsilon
    * failure, the parser will "rewind" to the point before `x` began.
    */
  def soft: Parser0.Soft[A] =
    new Parser0.Soft(this)

  /** Return a parser that succeeds (consuming nothing, and extracting
    * nothing) if the current parser would fail.
    *
    * This parser expects the underlying parser to fail, and will
    * unconditionally backtrack after running it.
    */
  def unary_! : Parser0[Unit] =
    Parser0.not(this)

  /** Return a parser that succeeds (consuming nothing and extracting
    * nothing) if the current parser would also succeed.
    *
    * This parser expects the underlying parser to succeed, and will
    * unconditionally backtrack after running it.
    */
  def peek: Parser0[Unit] =
    Parser0.peek(this)

  /** Use this parser to parse between values.
    *
    * Parses `b` followed by `this` and `c`.
    * Returns only the values extracted by `this` parser.
    */
  def between(b: Parser0[Any], c: Parser0[Any]): Parser0[A] =
    (b.void ~ (this ~ c.void)).map { case (_, (a, _)) => a }

  /** Use this parser to parse surrounded by values.
    *
    * This is the same as `between(b, b)`
    */
  def surroundedBy(b: Parser0[Any]): Parser0[A] =
    between(b, b)

  /** Internal (mutable) parsing method.
    *
    * This method should only be called internally by parser instances.
    */
  protected def parseMut(state: Parser0.Impl.State): A
}

/** Parser[A] is a Parser0[A] that will always consume one-or-more
  * characters on a successful parse.
  *
  * Since Parser is guaranteed to consume input it provides additional
  * methods which would be unsafe when used on parsers that succeed
  * without consuming input, such as `rep0`.
  *
  * When a Parser is composed with a Parser0 the result is usually a
  * Parser. Parser overrides many of Parser0's methods to refine the
  * return type. In other cases, callers may need to use the `with1`
  * helper method to refine the type of their expressions.
  *
  * Parser doesn't provide any additional guarantees over Parser0 on
  * what kind of parsing failures it can return.
  */
sealed abstract class Parser[+A] extends Parser0[A] {

  /** This method overrides `Parser0#filter` to refine the return type.
    */
  override def filter(fn: A => Boolean): Parser[A] = {
    val leftUnit = Left(())
    Parser0.select(this.map { a =>
      if (fn(a)) Right(a)
      else leftUnit
    })(Parser0.Fail)
  }

  /** This method overrides `Parser0#void` to refine the return type.
    */
  override def void: Parser[Unit] =
    Parser0.void(this)

  /** This method overrides `Parser0#string` to refine the return type.
    */
  override def string: Parser[String] =
    Parser0.string(this)

  /** This method overrides `Parser0#backtrack` to refine the return type.
    */
  override def backtrack: Parser[A] =
    Parser0.backtrack(this)

  /** This method overrides `Parser0#~` to refine the return type.
    */
  override def ~[B](that: Parser0[B]): Parser[(A, B)] =
    Parser0.product(this, that)

  /** Compose two parsers, ignoring the values extracted by the
    * left-hand parser.
    *
    * `x *> y` is equivalent to `(x.void ~ y).map(_._2)`.
    */
  def *>[B](that: Parser0[B]): Parser[B] =
    (void ~ that).map(_._2)

  /** Compose two parsers, ignoring the values extracted by the
    * right-hand parser.
    *
    * `x <* y` is equivalent to `(x ~ y.void).map(_._1)`.
    */
  def <*[B](that: Parser0[B]): Parser[A] =
    (this ~ that.void).map(_._1)

  /** This method overrides `Parser0#collect` to refine the return type.
    */
  override def collect[B](fn: PartialFunction[A, B]): Parser[B] =
    mapFilter(fn.lift)

  /** This method overrides `Parser0#map` to refine the return type.
    */
  override def map[B](fn: A => B): Parser[B] =
    Parser0.map(this)(fn)

  /** This method overrides `Parser0#mapFilter` to refine the return type.
    */
  override def mapFilter[B](fn: A => Option[B]): Parser[B] = {
    val leftUnit = Left(())

    val first = map { a =>
      fn(a) match {
        case Some(b) => Right(b)
        case None => leftUnit
      }
    }
    Parser0.select(first)(Parser0.Fail)
  }

  /** This method overrides `Parser0#flatMap` to refine the return type.
    */
  override def flatMap[B](fn: A => Parser0[B]): Parser[B] =
    Parser0.flatMap(this)(fn)

  /** This method overrides `Parser0#as` to refine the return type.
    */
  override def as[B](b: B): Parser[B] =
    Parser0.as(this, b)

  /** If this parser fails to parse its input with an epsilon error,
    * try the given parser instead.
    *
    * This method is similar to Parser0#orElse, but since both arguments
    * are known to be Parser values, the result is known to be a
    * Parser as well.
    */
  def orElse[A1 >: A](that: Parser[A1]): Parser[A1] =
    Parser0.oneOf(this :: that :: Nil)

  /** Use this parser to parse zero-or-more values.
    *
    * This parser may succeed without consuming input in the case where
    * zero values are parsed.
    *
    * If the underlying parser hits an arresting failure, the entire
    * parse is also an arresting failure. If the underlying parser hits
    * an epsilon failure, the parsed values (if any) are returned in a
    * list as a successful parse.
    */
  def rep0: Parser0[List[A]] =
    Parser0.rep0(this)

  /** Use this parser to parse at least `min` values (where `min >= 0`).
    *
    * If `min` is zero, this parser may succeed without consuming
    * input in the case where zero values are parsed. If `min` is
    * known to be greater than zero, consider using `rep(min)`
    * instead.
    *
    * Like `rep0`, arresting failures in the underlying parser will
    * result in an arresting failure. Unlike `rep0`, this method may
    * also return an arresting failure if it has not parsed at least
    * `min` values (but has consumed input).
    */
  def rep0(min: Int): Parser0[List[A]] =
    if (min == 0) rep0
    else rep(min).map(_.toList)

  /** Use this parser to parse one-or-more values.
    *
    * This parser behaves like `rep0`, except that it must produce at
    * least one value, and is guaranteed to consume input on successful
    * parses.
    */
  def rep: Parser[NonEmptyList[A]] =
    Parser0.rep(this, min = 1)

  /** Use this parser to parse at least `min` values (where `min >= 1`).
    *
    * This method behaves likes `rep`, except that if fewer than `min`
    * values are produced an arresting failure will be returned.
    */
  def rep(min: Int): Parser[NonEmptyList[A]] =
    Parser0.rep(this, min = min)

  /** This method overrides `Parser0#between` to refine the return type
    */
  override def between(b: Parser0[Any], c: Parser0[Any]): Parser[A] =
    (b.void.with1 ~ (this ~ c.void)).map { case (_, (a, _)) => a }

  /** This method overrides `Parser0#surroundedBy` to refine the return type
    */
  override def surroundedBy(b: Parser0[Any]): Parser[A] =
    between(b, b)

  /** This method overrides `Parser0#soft` to refine the return type.
    */
  override def soft: Parser0.Soft10[A] =
    new Parser0.Soft10(this)
}

object Parser0 extends ParserInstances {

  /** An expectation reports the kind or parsing error
    * and where it occured.
    */
  sealed abstract class Expectation {
    def offset: Int
  }

  object Expectation {
    case class Str(offset: Int, str: String) extends Expectation
    // expected a character in a given range
    case class InRange(offset: Int, lower: Char, upper: Char) extends Expectation
    case class StartOfString(offset: Int) extends Expectation
    case class EndOfString(offset: Int, length: Int) extends Expectation
    case class Length(offset: Int, expected: Int, actual: Int) extends Expectation
    case class ExpectedFailureAt(offset: Int, matched: String) extends Expectation
    // this is the result of oneOf0(Nil) at a given location
    case class Fail(offset: Int) extends Expectation
    case class FailWith(offset: Int, message: String) extends Expectation

    implicit val catsOrderExpectation: Order[Expectation] =
      new Order[Expectation] {
        def compare(left: Expectation, right: Expectation): Int = {
          val c = Integer.compare(left.offset, right.offset)
          if (c != 0) c
          else if (left == right) 0
          else {
            // these are never equal
            (left, right) match {
              case (Str(_, s1), Str(_, s2)) => s1.compare(s2)
              case (Str(_, _), _) => -1
              case (InRange(_, _, _), Str(_, _)) => 1
              case (InRange(_, l1, u1), InRange(_, l2, u2)) =>
                val c1 = Character.compare(l1, l2)
                if (c1 == 0) Character.compare(u1, u2)
                else c1
              case (InRange(_, _, _), _) => -1
              case (StartOfString(_), Str(_, _) | InRange(_, _, _)) => 1
              case (StartOfString(_), _) =>
                -1 // if they have the same offset, already handled above
              case (EndOfString(_, _), Str(_, _) | InRange(_, _, _) | StartOfString(_)) => 1
              case (EndOfString(_, l1), EndOfString(_, l2)) =>
                Integer.compare(l1, l2)
              case (EndOfString(_, _), _) => -1
              case (
                    Length(_, _, _),
                    Str(_, _) | InRange(_, _, _) | StartOfString(_) | EndOfString(_, _)
                  ) =>
                1
              case (Length(_, e1, a1), Length(_, e2, a2)) =>
                val c1 = Integer.compare(e1, e2)
                if (c1 == 0) Integer.compare(a1, a2)
                else c1
              case (Length(_, _, _), _) => -1
              case (ExpectedFailureAt(_, _), Fail(_)) => -1
              case (ExpectedFailureAt(_, _), FailWith(_, _)) => -1
              case (ExpectedFailureAt(_, m1), ExpectedFailureAt(_, m2)) =>
                m1.compare(m2)
              case (ExpectedFailureAt(_, _), _) => 1
              case (Fail(_), FailWith(_, _)) => -1
              case (Fail(_), _) => 1
              case (FailWith(_, s1), FailWith(_, s2)) =>
                s1.compare(s2)
              case (FailWith(_, _), _) => 1
            }
          }
        }
      }

    /** Sort, dedup and unify ranges for the errors accumulated
      * This is called just before finally returning an error in Parser0.parse
      */
    def unify(errors: NonEmptyList[Expectation]): NonEmptyList[Expectation] = {
      val el = errors.toList

      // merge all the ranges:
      val rangeMerge: List[InRange] =
        el
          .collect { case InRange(o, l, u) => (o, l to u) }
          .groupBy(_._1)
          .iterator
          .flatMap { case (o, ranges) =>
            // TODO: this could be optimized to not enumerate the set
            // for instance, a cheap thing to do is see if they
            // overlap or not
            val ary = ranges.iterator.map(_._2).flatten.toArray
            java.util.Arrays.sort(ary)
            Impl.rangesFor(ary).map { case (l, u) => InRange(o, l, u) }.toList
          }
          .toList

      // we don't need Fails that point to duplicate offsets
      val nonFailOffsets: Set[Int] =
        el.iterator.filterNot(_.isInstanceOf[Fail]).map(_.offset).toSet

      val errors1 = NonEmptyList.fromListUnsafe(
        el.filterNot {
          case Fail(off) => nonFailOffsets(off)
          case _ => false
        }
      )

      if (rangeMerge.isEmpty) errors1.distinct.sorted
      else {
        val nonRanges = errors1.toList.filterNot(_.isInstanceOf[InRange])

        NonEmptyList
          .fromListUnsafe(
            (rangeMerge reverse_::: nonRanges).distinct
          )
          .sorted
      }
    }
  }

  /** Represents where a failure occurred and all the expectations that were broken
    */
  final case class Error(failedAtOffset: Int, expected: NonEmptyList[Expectation]) {
    def offsets: NonEmptyList[Int] =
      expected.map(_.offset).distinct
  }

  /** Enables syntax to access product01, product and flatMap01
    *  This helps us build Parser instances when starting from
    *  a Parser0
    */
  final class With1[+A](val parser: Parser0[A]) extends AnyVal {

    /** parser then that.
      *  Since that is a Parser the result is
      */
    def ~[B](that: Parser[B]): Parser[(A, B)] =
      Parser0.product01(parser, that)

    /** This is the usual monadic composition, but you
      * should much prefer to use ~ or Apply.product, *>, <*, etc
      * if you can since it is much more efficient. This
      * has to call fn on each parse, which could be a lot
      * of extra work is you already know the result as is
      * the case for ~
      */
    def flatMap[B](fn: A => Parser[B]): Parser[B] =
      Parser0.flatMap01(parser)(fn)

    /** parser then that.
      *  Since that is a Parser the result is
      */
    def *>[B](that: Parser[B]): Parser[B] =
      product01(void0(parser), that).map(_._2)

    /** parser then that.
      *  Since that is a Parser the result is
      */
    def <*[B](that: Parser[B]): Parser[A] =
      product01(parser, void(that)).map(_._1)

    /** If we can parse this then that, do so,
      * if we fail that without consuming, rewind
      * before this without consuming.
      * If either consume 1 or more, do not rewind
      */
    def soft: Soft01[A] =
      new Soft01(parser)

    /** parse between values.
      *  Since values are `Parser` the result is
      */
    def between(b: Parser[Any], c: Parser[Any]): Parser[A] =
      (b.void ~ (parser ~ c.void)).map { case (_, (a, _)) => a }

    /** parse surrounded by that.
      *  Since that is a Parser the result is
      */
    def surroundedBy(that: Parser[Any]): Parser[A] =
      between(that, that)
  }

  /** If we can parse this then that, do so,
    * if we fail that without consuming, rewind
    * before this without consuming.
    * If either consume 1 or more, do not rewind
    */
  sealed class Soft[+A](parser: Parser0[A]) {
    def ~[B](that: Parser0[B]): Parser0[(A, B)] =
      softProduct0(parser, that)

    def *>[B](that: Parser0[B]): Parser0[B] =
      softProduct0(void0(parser), that).map(_._2)

    def <*[B](that: Parser0[B]): Parser0[A] =
      softProduct0(parser, void0(that)).map(_._1)

    /** If we can parse this then that, do so,
      * if we fail that without consuming, rewind
      * before this without consuming.
      * If either consume 1 or more, do not rewind
      */
    def with1: Soft01[A] =
      new Soft01(parser)
  }

  /** If we can parse this then that, do so,
    * if we fail that without consuming, rewind
    * before this without consuming.
    * If either consume 1 or more, do not rewind
    */
  final class Soft10[+A](parser: Parser[A]) extends Soft(parser) {
    override def ~[B](that: Parser0[B]): Parser[(A, B)] =
      softProduct(parser, that)

    override def *>[B](that: Parser0[B]): Parser[B] =
      softProduct(void(parser), that).map(_._2)

    override def <*[B](that: Parser0[B]): Parser[A] =
      softProduct(parser, void0(that)).map(_._1)
  }

  /** If we can parse this then that, do so,
    * if we fail that without consuming, rewind
    * before this without consuming.
    * If either consume 1 or more, do not rewind
    */
  final class Soft01[+A](val parser: Parser0[A]) extends AnyVal {
    def ~[B](that: Parser[B]): Parser[(A, B)] =
      softProduct01(parser, that)

    def *>[B](that: Parser[B]): Parser[B] =
      softProduct01(void0(parser), that).map(_._2)

    def <*[B](that: Parser[B]): Parser[A] =
      softProduct01(parser, void(that)).map(_._1)
  }

  /** Methods with complex variance type signatures due to covariance.
    */
  implicit final class ParserMethods[A](private val self: Parser[A]) extends AnyVal {
    def repAs0[B](implicit acc: Accumulator0[A, B]): Parser0[B] =
      Parser0.repAs0(self)(acc)

    def repAs[B](implicit acc: Accumulator[A, B]): Parser[B] =
      Parser0.repAs(self, min = 1)(acc)

    def repAs[B](min: Int)(implicit acc: Accumulator[A, B]): Parser[B] =
      Parser0.repAs(self, min = min)(acc)
  }

  /** Don't advance in the parsed string, just return a
    *  This is used by the Applicative typeclass.
    */
  def pure[A](a: A): Parser0[A] =
    Impl.Pure(a)

  /** Parse a given string, in a case-insensitive manner,
    * or fail. This backtracks on failure
    * this is an error if the string is empty
    */
  def ignoreCase(str: String): Parser[Unit] =
    if (str.length == 1) {
      ignoreCaseChar(str.charAt(0))
    } else Impl.IgnoreCase(str.toLowerCase)

  /** Ignore the case of a single character
    *  If you want to know if it is upper or
    *  lower, use .string to capture the string
    *  and then map to process the result.
    */
  def ignoreCaseChar(c: Char): Parser[Unit] =
    charIn(c.toLower, c.toUpper).void

  /** Parse a given string or
    * fail. This backtracks on failure
    * this is an error if the string is empty
    */
  def string(str: String): Parser[Unit] =
    if (str.length == 1) char(str.charAt(0))
    else Impl.Str(str)

  /** Parse a potentially empty string or
    * fail. This backtracks on failure
    */
  def string0(str: String): Parser0[Unit] =
    if (str.length == 0) unit
    else string(str)

  /** Parse a potentially empty string, in a case-insensitive manner,
    * or fail. This backtracks on failure
    */
  def ignoreCase0(str: String): Parser0[Unit] =
    if (str.length == 0) unit
    else ignoreCase(str)

  /** go through the list of parsers trying each
    *  as long as they are epsilon failures (don't advance)
    *  see @backtrack if you want to do backtracking.
    *
    *  This is the same as parsers.foldLeft(fail)(_.orElse(_))
    */
  def oneOf[A](parsers: List[Parser[A]]): Parser[A] = {
    @annotation.tailrec
    def flatten(ls: List[Parser[A]], acc: ListBuffer[Parser[A]]): List[Parser[A]] =
      ls match {
        case Nil => acc.toList.distinct
        case Impl.OneOf(ps) :: rest =>
          flatten(ps ::: rest, acc)
        case Impl.Fail() :: rest =>
          flatten(rest, acc)
        case notOneOf :: rest =>
          flatten(rest, acc += notOneOf)
      }

    val flat = flatten(parsers, new ListBuffer)
    Impl.mergeCharIn[A, Parser[A]](flat) match {
      case Nil => fail
      case p :: Nil => p
      case two => Impl.OneOf(two)
    }
  }

  /** go through the list of parsers trying each
    *  as long as they are epsilon failures (don't advance)
    *  see @backtrack if you want to do backtracking.
    *
    *  This is the same as parsers.foldLeft(fail)(_.orElse0(_))
    */
  def oneOf0[A](ps: List[Parser0[A]]): Parser0[A] = {
    @annotation.tailrec
    def flatten(ls: List[Parser0[A]], acc: ListBuffer[Parser0[A]]): List[Parser0[A]] =
      ls match {
        case Nil => acc.toList.distinct
        case Impl.OneOf0(ps) :: rest =>
          flatten(ps ::: rest, acc)
        case Impl.OneOf(ps) :: rest =>
          flatten(ps ::: rest, acc)
        case Impl.Fail() :: rest =>
          flatten(rest, acc)
        case notOneOf :: rest =>
          if (Impl.alwaysSucceeds(notOneOf)) {
            (acc += notOneOf).toList.distinct
          } else {
            flatten(rest, acc += notOneOf)
          }
      }

    val flat = flatten(ps, new ListBuffer)
    Impl.mergeCharIn[A, Parser0[A]](flat) match {
      case Nil => fail
      case p :: Nil => p
      case two => Impl.OneOf0(two)
    }
  }

  private[this] val emptyStringParser0: Parser0[String] =
    pure("")

  /** if len < 1, the same as pure("")
    * else length(len)
    */
  def length0(len: Int): Parser0[String] =
    if (len > 0) length(len) else emptyStringParser0

  /** Parse the next len characters where len > 0
    * if (len < 1) throw IllegalArgumentException
    */
  def length(len: Int): Parser[String] =
    Impl.Length(len)

  /** Repeat this parser 0 or more times
    * note: this can wind up parsing nothing
    */
  def rep0[A](p1: Parser[A]): Parser0[List[A]] =
    repAs0[A, List[A]](p1)

  /** Repeat this parser 0 or more times
    * note: this can wind up parsing nothing
    */
  def repAs0[A, B](p1: Parser[A])(implicit acc: Accumulator0[A, B]): Parser0[B] =
    Impl.Rep0(p1, acc)

  /** Repeat this parser 1 or more times
    */
  def rep[A](p1: Parser[A], min: Int): Parser[NonEmptyList[A]] =
    repAs[A, NonEmptyList[A]](p1, min)

  /** Repeat this parser 1 or more times
    */
  def repAs[A, B](p1: Parser[A], min: Int)(implicit acc: Accumulator[A, B]): Parser[B] =
    Impl.Rep(p1, min, acc)

  /** Repeat 1 or more times with a separator
    */
  def repSep[A](p1: Parser[A], min: Int, sep: Parser0[Any]): Parser[NonEmptyList[A]] = {
    if (min <= 0) throw new IllegalArgumentException(s"require min > 0, found: $min")

    val rest = (sep.void.with1.soft *> p1).rep0(min - 1)
    (p1 ~ rest).map { case (h, t) => NonEmptyList(h, t) }
  }

  /** Repeat 0 or more times with a separator
    */
  def rep0Sep[A](p1: Parser[A], min: Int, sep: Parser0[Any]): Parser0[List[A]] = {
    if (min <= 0) repSep(p1, 1, sep).?.map {
      case None => Nil
      case Some(nel) => nel.toList
    }
    else repSep(p1, min, sep).map(_.toList)
  }

  /** parse first then second
    */
  def product0[A, B](first: Parser0[A], second: Parser0[B]): Parser0[(A, B)] =
    first match {
      case f1: Parser[A] => product(f1, second)
      case _ =>
        second match {
          case s1: Parser[B] =>
            product01(first, s1)
          case _ => Impl.Prod0(first, second)
        }
    }

  /** product with the first argument being a Parser
    */
  def product[A, B](first: Parser[A], second: Parser0[B]): Parser[(A, B)] =
    Impl.Prod(first, second)

  /** product with the second argument being a Parser
    */
  def product01[A, B](first: Parser0[A], second: Parser[B]): Parser[(A, B)] =
    Impl.Prod(first, second)

  /** softProduct, a variant of product
    *  A soft product backtracks if the first succeeds and the second
    *  is an epsilon-failure. By contrast product will be a failure in
    *  that case
    *
    *  see @Parser0.soft
    */
  def softProduct0[A, B](first: Parser0[A], second: Parser0[B]): Parser0[(A, B)] =
    first match {
      case f1: Parser[A] => softProduct(f1, second)
      case _ =>
        second match {
          case s1: Parser[B] =>
            softProduct01(first, s1)
          case _ => Impl.SoftProd0(first, second)
        }
    }

  /** softProduct with the first argument being a Parser
    *  A soft product backtracks if the first succeeds and the second
    *  is an epsilon-failure. By contrast product will be a failure in
    *  that case
    *
    *  see @Parser0.soft
    */
  def softProduct[A, B](first: Parser[A], second: Parser0[B]): Parser[(A, B)] =
    Impl.SoftProd(first, second)

  /** softProduct with the second argument being a Parser
    *  A soft product backtracks if the first succeeds and the second
    *  is an epsilon-failure. By contrast product will be a failure in
    *  that case
    *
    *  see @Parser0.soft
    */
  def softProduct01[A, B](first: Parser0[A], second: Parser[B]): Parser[(A, B)] =
    Impl.SoftProd(first, second)

  /** transform a Parser0 result
    */
  def map0[A, B](p: Parser0[A])(fn: A => B): Parser0[B] =
    p match {
      case p1: Parser[A] => map(p1)(fn)
      case Impl.Map(p0, f0) =>
        Impl.Map(p0, AndThen(f0).andThen(fn))
      case _ => Impl.Map(p, fn)
    }

  /** transform a Parser result
    */
  def map[A, B](p: Parser[A])(fn: A => B): Parser[B] =
    p match {
      case Impl.Map1(p0, f0) =>
        Impl.Map1(p0, AndThen(f0).andThen(fn))
      case Impl.Fail() | Impl.FailWith(_) =>
        // these are really Parser[Nothing{
        // but scala can't see that, so we cast
        p.asInstanceOf[Parser[B]]
      case _ => Impl.Map1(p, fn)
    }

  /** Parse p and if we get the Left side, parse fn
    *  This function name comes from seletive functors.
    *  This should be more efficient than flatMap since
    *  the fn Parser0 is evaluated once, not on every item
    *  parsed
    */
  def select0[A, B](p: Parser0[Either[A, B]])(fn: Parser0[A => B]): Parser0[B] =
    Impl
      .Select0(p, fn)
      .map {
        case Left((a, fn)) => fn(a)
        case Right(b) => b
      }

  /** Parser version of select
    */
  def select[A, B](p: Parser[Either[A, B]])(fn: Parser0[A => B]): Parser[B] =
    Impl
      .Select(p, fn)
      .map {
        case Left((a, fn)) => fn(a)
        case Right(b) => b
      }

  /** Standard monadic flatMap
    *  Avoid this function if possible. If you can
    *  instead use product, ~, *>, or <* use that.
    *  flatMap always has to allocate a parser, and the
    *  parser is less amenable to optimization
    */
  def flatMap0[A, B](pa: Parser0[A])(fn: A => Parser0[B]): Parser0[B] =
    Impl.FlatMap0(pa, fn)

  /** Standard monadic flatMap where you start with a Parser
    *  Avoid this function if possible. If you can
    *  instead use product, ~, *>, or <* use that.
    *  flatMap always has to allocate a parser, and the
    *  parser is less amenable to optimization
    */
  def flatMap[A, B](pa: Parser[A])(fn: A => Parser0[B]): Parser[B] =
    Impl.FlatMap(pa, fn)

  /** Standard monadic flatMap where you end with a Parser
    *  Avoid this function if possible. If you can
    *  instead use product, ~, *>, or <* use that.
    *  flatMap always has to allocate a parser, and the
    *  parser is less amenable to optimization
    */
  def flatMap01[A, B](pa: Parser0[A])(fn: A => Parser[B]): Parser[B] =
    Impl.FlatMap(pa, fn)

  /** tail recursive monadic flatMaps
    * This is a rarely used function, but needed to implement cats.FlatMap
    *  Avoid this function if possible. If you can
    *  instead use product, ~, *>, or <* use that.
    *  flatMap always has to allocate a parser, and the
    *  parser is less amenable to optimization
    */
  def tailRecM[A, B](init: A)(fn: A => Parser0[Either[A, B]]): Parser0[B] =
    Impl.TailRecM(init, fn)

  /** tail recursive monadic flatMaps on Parser
    * This is a rarely used function, but needed to implement cats.FlatMap
    *  Avoid this function if possible. If you can
    *  instead use product, ~, *>, or <* use that.
    *  flatMap always has to allocate a parser, and the
    *  parser is less amenable to optimization
    */
  def tailRecM1[A, B](init: A)(fn: A => Parser[Either[A, B]]): Parser[B] =
    Impl.TailRecM1(init, fn)

  /** Lazily create a Parser
    *  This is useful to create some recursive parsers
    *  see Defer0[Parser].fix
    */
  def defer[A](pa: => Parser[A]): Parser[A] =
    Impl.Defer(() => pa)

  /** Lazily create a Parser0
    *  This is useful to create some recursive parsers
    *  see Defer0[Parser].fix
    */
  def defer0[A](pa: => Parser0[A]): Parser0[A] =
    Impl.Defer0(() => pa)

  /** A parser that always fails with an epsilon failure
    */
  val Fail: Parser[Nothing] = Impl.Fail()

  /** A parser that always fails with an epsilon failure
    */
  def fail[A]: Parser[A] = Fail

  /** A parser that always fails with an epsilon failure and a given message
    * this is generally used with flatMap to validate a result beyond
    * the literal parsing.
    *
    * e.g. parsing a number then validate that it is bounded.
    */
  def failWith[A](message: String): Parser[A] =
    Impl.FailWith(message)

  /** A parser that returns unit
    */
  val unit: Parser0[Unit] = pure(())

  /** Parse 1 character from the string
    */
  def anyChar: Parser[Char] =
    Impl.AnyChar

  /** An empty iterable is the same as fail
    */
  def charIn(cs: Iterable[Char]): Parser[Char] =
    if (cs.isEmpty) fail
    else {
      val ary = cs.toArray
      java.util.Arrays.sort(ary)
      Impl.rangesFor(ary) match {
        case NonEmptyList((low, high), Nil) if low == Char.MinValue && high == Char.MaxValue =>
          anyChar
        case notAnyChar =>
          Impl.CharIn(ary(0).toInt, BitSetUtil.bitSetFor(ary), notAnyChar)
      }
    }

  /** Parse any single character in a set of characters as lower or upper case
    */
  def ignoreCaseCharIn(cs: Iterable[Char]): Parser[Char] = {
    val letters = cs.flatMap { c => c.toUpper :: c.toLower :: Nil }
    charIn(letters)
  }

  /** Parse any single character in a set of characters as lower or upper case
    */
  def ignoreCaseCharIn(c0: Char, cs: Char*): Parser[Char] =
    ignoreCaseCharIn(c0 +: cs)

  @inline
  private[this] def charImpl(c: Char): Parser[Unit] =
    charIn(c :: Nil).void

  // Cache the common parsers to reduce allocations
  private[this] val charArray: Array[Parser[Unit]] =
    (32 to 126).map { idx => charImpl(idx.toChar) }.toArray

  /** parse a single character
    */
  def char(c: Char): Parser[Unit] = {
    val cidx = c.toInt - 32
    if ((cidx >= 0) && (cidx < charArray.length)) charArray(cidx)
    else charImpl(c)
  }

  /** parse one of a given set of characters
    */
  def charIn(c0: Char, cs: Char*): Parser[Char] =
    charIn(c0 +: cs)

  /** parse one character that matches a given function
    */
  def charWhere(fn: Char => Boolean): Parser[Char] =
    charIn(Impl.allChars.filter(fn))

  /** Parse a string while the given function is true
    */
  def charsWhile(fn: Char => Boolean): Parser0[String] =
    charWhere(fn).rep0.string

  /** Parse a string while the given function is true
    * parses at least one character
    */
  def charsWhile1(fn: Char => Boolean): Parser[String] =
    charWhere(fn).rep.string

  /** parse zero or more characters as long as they don't match p
    */
  def until(p: Parser0[Any]): Parser0[String] =
    (not(p).with1 ~ anyChar).rep0.string

  /** parse one or more characters as long as they don't match p
    */
  def until1(p: Parser0[Any]): Parser[String] =
    (not(p).with1 ~ anyChar).rep.string

  /** discard the value in a Parser0.
    *  This is an optimization because we remove trailing
    *  map operations and don't allocate internal data structures
    *  This function is called internal to Functor.as and Apply.*>
    *  and Apply.<* so those are good uses.
    */
  def void0(pa: Parser0[Any]): Parser0[Unit] =
    pa match {
      case v @ Impl.Void0(_) => v
      case p1: Parser[_] => void(p1)
      case s if Impl.alwaysSucceeds(s) => unit
      case _ =>
        Impl.unmap0(pa) match {
          case Impl.StartParser0 => Impl.StartParser0
          case Impl.EndParser0 => Impl.EndParser0
          case n @ Impl.Not(_) => n
          case p @ Impl.Peek(_) => p
          case other => Impl.Void0(other)
        }
    }

  /** discard the value in a Parser.
    *  This is an optimization because we remove trailing
    *  map operations and don't allocate internal data structures
    *  This function is called internal to Functor.as and Apply.*>
    *  and Apply.<* so those are good uses.
    */
  def void(pa: Parser[Any]): Parser[Unit] =
    pa match {
      case v @ Impl.Void(_) => v
      case _ =>
        Impl.unmap(pa) match {
          case f @ (Impl.Fail() | Impl.FailWith(_)) =>
            // these are really Parser[Nothing]
            // but scala can't see that, so we cast
            f.asInstanceOf[Parser[Unit]]
          case p: Impl.Str => p
          case notVoid => Impl.Void(notVoid)
        }
    }

  /** Discard the result A and instead capture the matching string
    *  this is optimized to avoid internal allocations
    */
  def string0(pa: Parser0[Any]): Parser0[String] =
    pa match {
      case str @ Impl.StringP(_) => str
      case s1: Parser[_] => string(s1)
      case _ =>
        Impl.unmap0(pa) match {
          case Impl.Pure(_) | Impl.Index => emptyStringParser0
          case notEmpty => Impl.StringP(notEmpty)
        }
    }

  /** Discard the result A and instead capture the matching string
    *  this is optimized to avoid internal allocations
    */
  def string(pa: Parser[Any]): Parser[String] =
    pa match {
      case str @ Impl.StringP1(_) => str
      case _ =>
        Impl.unmap(pa) match {
          case len @ Impl.Length(_) => len
          case strP @ Impl.Str(expect) => strP.as(expect)
          case ci @ Impl.CharIn(min, bs, _) if BitSetUtil.isSingleton(bs) =>
            // we can allocate the returned string once here
            val minStr = min.toChar.toString
            ci.as(minStr)
          case f @ (Impl.Fail() | Impl.FailWith(_)) =>
            // these are really Parser[Nothing]
            // but scala can't see that, so we cast
            f.asInstanceOf[Parser[String]]
          case notStr => Impl.StringP1(notStr)
        }
    }

  /** returns a parser that succeeds if the
    * current parser fails.
    * Note, this parser backtracks (never returns an arresting failure)
    */
  def not(pa: Parser0[Any]): Parser0[Unit] =
    void0(pa) match {
      case Impl.Fail() | Impl.FailWith(_) => unit
      case notFail => Impl.Not(notFail)
    }

  /** a parser that consumes nothing when
    * it succeeds, basically rewind on success
    */
  def peek(pa: Parser0[Any]): Parser0[Unit] =
    pa match {
      case peek @ Impl.Peek(_) => peek
      case s if Impl.alwaysSucceeds(s) => unit
      case notPeek =>
        // TODO: we can adjust Rep0/Rep to do minimal
        // work since we rewind after we are sure there is
        // a match
        Impl.Peek(void0(notPeek))
    }

  /** return the current position in the string
    * we are parsing. This lets you record position information
    * in your ASTs you are parsing
    */
  def index: Parser0[Int] = Impl.Index

  /** succeeds when we are at the start
    */
  def start: Parser0[Unit] = Impl.StartParser0

  /** succeeds when we are at the end
    */
  def end: Parser0[Unit] = Impl.EndParser0

  /** If we fail, rewind the offset back so that
    * we can try other branches. This tends
    * to harm debuggability and ideally should be
    * minimized
    */
  def backtrack0[A](pa: Parser0[A]): Parser0[A] =
    pa match {
      case p1: Parser[A] => backtrack(p1)
      case pa if Impl.doesBacktrack(pa) => pa
      case nbt => Impl.Backtrack0(nbt)
    }

  /** If we fail, rewind the offset back so that
    * we can try other branches. This tends
    * to harm debuggability and ideally should be
    * minimized
    */
  def backtrack[A](pa: Parser[A]): Parser[A] =
    pa match {
      case pa if Impl.doesBacktrack(pa) => pa
      case nbt => Impl.Backtrack(nbt)
    }

  /** Replaces parsed values with the given value.
    */
  def as0[A, B](pa: Parser0[A], b: B): Parser0[B] =
    pa match {
      case Impl.Pure(_) | Impl.Index => pure(b)
      case p1: Parser[A] => as(p1, b)
      case _ => pa.void.map(Impl.ConstFn(b))
    }

  /** Replaces parsed values with the given value.
    */
  def as[A, B](pa: Parser[A], b: B): Parser[B] =
    (pa.void, b) match {
      case (Impl.Void(ci @ Impl.CharIn(min, bs, _)), bc: Char)
          if BitSetUtil.isSingleton(bs) && (min.toChar == bc) =>
        // this is putting the character back on a singleton CharIn, just return the char in
        ci.asInstanceOf[Parser[B]]
      case (notSingleChar, _) => notSingleChar.map(Impl.ConstFn(b))
    }

  implicit val catsInstancesParser
      : FlatMap[Parser] with Defer[Parser] with MonoidK[Parser] with FunctorFilter[Parser] =
    new FlatMap[Parser] with Defer[Parser] with MonoidK[Parser] with FunctorFilter[Parser] {
      def empty[A] = Fail

      def defer[A](pa: => Parser[A]): Parser[A] =
        Parser0.this.defer(pa)

      def functor = this

      def map[A, B](fa: Parser[A])(fn: A => B): Parser[B] =
        Parser0.this.map(fa)(fn)

      def mapFilter[A, B](fa: Parser[A])(f: A => Option[B]): Parser[B] =
        fa.mapFilter(f)

      override def filter[A](fa: Parser[A])(fn: A => Boolean): Parser[A] =
        fa.filter(fn)

      override def filterNot[A](fa: Parser[A])(fn: A => Boolean): Parser[A] =
        fa.filter { a => !fn(a) }

      def flatMap[A, B](fa: Parser[A])(fn: A => Parser[B]): Parser[B] =
        Parser0.this.flatMap(fa)(fn)

      override def product[A, B](pa: Parser[A], pb: Parser[B]): Parser[(A, B)] =
        Parser0.this.product(pa, pb)

      override def map2[A, B, C](pa: Parser[A], pb: Parser[B])(fn: (A, B) => C): Parser[C] =
        map(product(pa, pb)) { case (a, b) => fn(a, b) }

      override def map2Eval[A, B, C](pa: Parser[A], pb: Eval[Parser[B]])(
          fn: (A, B) => C
      ): Eval[Parser[C]] =
        Now(pb match {
          case Now(pb) => map2(pa, pb)(fn)
          case later => map2(pa, defer(later.value))(fn)
        })

      override def ap[A, B](pf: Parser[A => B])(pa: Parser[A]): Parser[B] =
        map(product(pf, pa)) { case (fn, a) => fn(a) }

      def tailRecM[A, B](init: A)(fn: A => Parser[Either[A, B]]): Parser[B] =
        tailRecM1(init)(fn)

      def combineK[A](pa: Parser[A], pb: Parser[A]): Parser[A] =
        Parser0.oneOf(pa :: pb :: Nil)

      override def void[A](pa: Parser[A]): Parser[Unit] =
        pa.void

      override def as[A, B](pa: Parser[A], b: B): Parser[B] =
        Parser0.as(pa, b)

      override def productL[A, B](pa: Parser[A])(pb: Parser[B]): Parser[A] =
        map(product(pa, pb.void)) { case (a, _) => a }

      override def productR[A, B](pa: Parser[A])(pb: Parser[B]): Parser[B] =
        map(product(pa.void, pb)) { case (_, b) => b }

      override def productLEval[A, B](fa: Parser[A])(fb: Eval[Parser[B]]): Parser[A] = {
        val pb =
          fb match {
            case Now(pb) => pb
            case notNow => defer(notNow.value)
          }

        productL(fa)(pb)
      }

      override def productREval[A, B](fa: Parser[A])(fb: Eval[Parser[B]]): Parser[B] = {
        val pb =
          fb match {
            case Now(pb) => pb
            case notNow => defer(notNow.value)
          }

        productR(fa)(pb)
      }

    }

  private object Impl {

    val allChars = Char.MinValue to Char.MaxValue

    val optTail: List[Parser0[Option[Nothing]]] = Parser0.pure(None) :: Nil

    case class ConstFn[A](result: A) extends Function[Any, A] {
      def apply(any: Any) = result
    }

    final def doesBacktrackCheat(p: Parser0[Any]): Boolean =
      doesBacktrack(p)

    @annotation.tailrec
    final def doesBacktrack(p: Parser0[Any]): Boolean =
      p match {
        case Backtrack0(_) | Backtrack(_) | AnyChar | CharIn(_, _, _) | Str(_) | IgnoreCase(_) |
            Length(_) | StartParser0 | EndParser0 | Index | Pure(_) | Fail() | FailWith(_) | Not(_) =>
          true
        case Map(p, _) => doesBacktrack(p)
        case Map1(p, _) => doesBacktrack(p)
        case SoftProd0(a, b) => doesBacktrackCheat(a) && doesBacktrack(b)
        case SoftProd(a, b) => doesBacktrackCheat(a) && doesBacktrack(b)
        case _ => false
      }

    // does this parser always succeed?
    // note: a parser1 does not always succeed
    // and by construction, a oneOf0 never always succeeds
    final def alwaysSucceeds(p: Parser0[Any]): Boolean =
      p match {
        case Index | Pure(_) => true
        case Map(p, _) => alwaysSucceeds(p)
        case SoftProd0(a, b) => alwaysSucceeds(a) && alwaysSucceeds(b)
        case Prod0(a, b) => alwaysSucceeds(a) && alwaysSucceeds(b)
        // by construction we never build a Not(Fail()) since
        // it would just be the same as unit
        //case Not(Fail() | FailWith(_)) => true
        case _ => false
      }

    /** This removes any trailing map functions which
      * can cause wasted allocations if we are later going
      * to void or return strings. This stops
      * at StringP or VoidP since those are markers
      * that anything below has already been transformed
      */
    def unmap0(pa: Parser0[Any]): Parser0[Any] =
      pa match {
        case p1: Parser[Any] => unmap(p1)
        case Pure(_) | Index => Parser0.unit
        case s if alwaysSucceeds(s) => Parser0.unit
        case Map(p, _) =>
          // we discard any allocations done by fn
          unmap0(p)
        case Select0(p, fn) =>
          Select0(p, unmap0(fn))
        case StringP(s) =>
          // StringP is added privately, and only after unmap0
          s
        case Void0(v) =>
          // Void is added privately, and only after unmap0
          v
        case n @ Not(_) =>
          // not is already voided
          n
        case p @ Peek(_) =>
          // peek is already voided
          p
        case Backtrack0(p) =>
          // unmap0 may simplify enough
          // to remove the backtrack wrapper
          Parser0.backtrack0(unmap0(p))
        case OneOf0(ps) => Parser0.oneOf0(ps.map(unmap0))
        case Prod0(p1, p2) =>
          unmap0(p1) match {
            case Prod0(p11, p12) =>
              // right associate so
              // we can check matches a bit faster
              // note: p12 is already unmapped, so
              // we wrap with Void to prevent n^2 cost
              Prod0(p11, unmap0(Prod0(Void0(p12), p2)))
            case u1 if u1 eq Parser0.unit =>
              unmap0(p2)
            case u1 =>
              val u2 = unmap0(p2)
              if (u2 eq Parser0.unit) u1
              else Prod0(u1, u2)
          }
        case SoftProd0(p1, p2) =>
          unmap0(p1) match {
            case SoftProd0(p11, p12) =>
              // right associate so
              // we can check matches a bit faster
              // note: p12 is already unmapped, so
              // we wrap with Void to prevent n^2 cost
              SoftProd0(p11, unmap0(SoftProd0(Void0(p12), p2)))
            case u1 if u1 eq Parser0.unit =>
              unmap0(p2)
            case u1 =>
              val u2 = unmap0(p2)
              if (u2 eq Parser0.unit) u1
              else SoftProd0(u1, u2)
          }
        case Defer0(fn) =>
          Defer0(() => unmap0(compute0(fn)))
        case Rep0(p, _) => Rep0(unmap(p), Accumulator0.unitAccumulator0)
        case StartParser0 | EndParser0 | TailRecM(_, _) | FlatMap0(_, _) =>
          // we can't transform this significantly
          pa
      }

    def expect1[A](p: Parser0[A]): Parser[A] =
      p match {
        case p1: Parser[A] => p1
        case notP1 =>
          // $COVERAGE-OFF$
          sys.error(s"violated invariant: $notP1 should be a Parser")
        // $COVERAGE-ON$
      }

    /** This removes any trailing map functions which
      * can cause wasted allocations if we are later going
      * to void or return strings. This stops
      * at StringP or VoidP since those are markers
      * that anything below has already been transformed
      */
    def unmap(pa: Parser[Any]): Parser[Any] =
      pa match {
        case Map1(p, _) =>
          // we discard any allocations done by fn
          unmap(p)
        case Select(p, fn) =>
          Select(p, unmap0(fn))
        case StringP1(s) =>
          // StringP is added privately, and only after unmap
          s
        case Void(v) =>
          // Void is added privately, and only after unmap
          v
        case Backtrack(p) =>
          // unmap may simplify enough
          // to remove the backtrack wrapper
          Parser0.backtrack(unmap(p))
        case OneOf(ps) => Parser0.oneOf(ps.map(unmap))
        case Prod(p1, p2) =>
          unmap0(p1) match {
            case Prod0(p11, p12) =>
              // right associate so
              // we can check matches a bit faster
              // note: p12 is already unmapped, so
              // we wrap with Void to prevent n^2 cost
              Prod(p11, unmap0(Parser0.product0(p12.void, p2)))
            case Prod(p11, p12) =>
              // right associate so
              // we can check matches a bit faster
              // we wrap with Void to prevent n^2 cost
              Prod(p11, unmap0(Parser0.product0(p12.void, p2)))
            case u1 if u1 eq Parser0.unit =>
              // if unmap0(u1) is unit, p2 must be a Parser
              unmap(expect1(p2))
            case u1 =>
              val u2 = unmap0(p2)
              if (u2 eq Parser0.unit) expect1(u1)
              else Prod(u1, u2)
          }
        case SoftProd(p1, p2) =>
          unmap0(p1) match {
            case SoftProd0(p11, p12) =>
              // right associate so
              // we can check matches a bit faster
              // we wrap with Void to prevent n^2 cost
              SoftProd(p11, unmap0(Parser0.softProduct0(p12.void, p2)))
            case SoftProd(p11, p12) =>
              // right associate so
              // we can check matches a bit faster
              // we wrap with Void to prevent n^2 cost
              SoftProd(p11, unmap0(Parser0.softProduct0(p12.void, p2)))
            case u1 if u1 eq Parser0.unit =>
              // if unmap0(u1) is unit, p2 must be a Parser
              unmap(expect1(p2))
            case u1 =>
              val u2 = unmap0(p2)
              if (u2 eq Parser0.unit) expect1(u1)
              else SoftProd(u1, u2)
          }
        case Defer(fn) =>
          Defer(() => unmap(compute(fn)))
        case Rep(p, m, _) => Rep(unmap(p), m, Accumulator0.unitAccumulator0)
        case AnyChar | CharIn(_, _, _) | Str(_) | IgnoreCase(_) | Fail() | FailWith(_) | Length(_) |
            TailRecM1(_, _) | FlatMap(_, _) =>
          // we can't transform this significantly
          pa

      }

    final class State(val str: String) {
      var offset: Int = 0
      var error: Chain[Expectation] = null
      var capture: Boolean = true
    }

    case class Pure[A](result: A) extends Parser0[A] {
      override def parseMut(state: State): A = result
    }

    case class Length(len: Int) extends Parser[String] {
      if (len < 1) throw new IllegalArgumentException(s"required length > 0, found $len")

      override def parseMut(state: State): String = {
        val offset = state.offset
        val end = offset + len
        if (end <= state.str.length) {
          val res = if (state.capture) state.str.substring(offset, end) else null
          state.offset = end
          res
        } else {
          state.error = Chain.one(Expectation.Length(offset, len, state.str.length - offset))
          null
        }
      }
    }

    def void(pa: Parser0[Any], state: State): Unit = {
      val s0 = state.capture
      state.capture = false
      pa.parseMut(state)
      state.capture = s0
      ()
    }

    case class Void0[A](parser: Parser0[A]) extends Parser0[Unit] {
      override def parseMut(state: State): Unit =
        Impl.void(parser, state)
    }

    case class Void[A](parser: Parser[A]) extends Parser[Unit] {
      override def parseMut(state: State): Unit =
        Impl.void(parser, state)
    }

    def string0(pa: Parser0[Any], state: State): String = {
      val s0 = state.capture
      state.capture = false
      val init = state.offset
      pa.parseMut(state)
      val str = state.str.substring(init, state.offset)
      state.capture = s0
      str
    }

    case class StringP[A](parser: Parser0[A]) extends Parser0[String] {
      override def parseMut(state: State): String =
        Impl.string0(parser, state)
    }

    case class StringP1[A](parser: Parser[A]) extends Parser[String] {
      override def parseMut(state: State): String =
        Impl.string0(parser, state)
    }

    case object StartParser0 extends Parser0[Unit] {
      override def parseMut(state: State): Unit = {
        if (state.offset != 0) {
          state.error = Chain.one(Expectation.StartOfString(state.offset))
        }
        ()
      }
    }

    case object EndParser0 extends Parser0[Unit] {
      override def parseMut(state: State): Unit = {
        if (state.offset != state.str.length) {
          state.error = Chain.one(Expectation.EndOfString(state.offset, state.str.length))
        }
        ()
      }
    }

    case object Index extends Parser0[Int] {
      override def parseMut(state: State): Int = state.offset
    }

    final def backtrack[A](pa: Parser0[A], state: State): A = {
      val offset = state.offset
      val a = pa.parseMut(state)
      if (state.error ne null) {
        state.offset = offset
      }
      a
    }

    case class Backtrack0[A](parser: Parser0[A]) extends Parser0[A] {
      override def parseMut(state: State): A =
        Impl.backtrack(parser, state)
    }

    case class Backtrack[A](parser: Parser[A]) extends Parser[A] {
      override def parseMut(state: State): A =
        Impl.backtrack(parser, state)
    }

    case class Str(message: String) extends Parser[Unit] {
      if (message.isEmpty)
        throw new IllegalArgumentException("we need a non-empty string to expect a message")

      override def parseMut(state: State): Unit = {
        val offset = state.offset
        if (state.str.regionMatches(offset, message, 0, message.length)) {
          state.offset += message.length
          ()
        } else {
          state.error = Chain.one(Expectation.Str(offset, message))
          ()
        }
      }
    }

    case class IgnoreCase(message: String) extends Parser[Unit] {
      if (message.isEmpty)
        throw new IllegalArgumentException("we need a non-empty string to expect a message")

      override def parseMut(state: State): Unit = {
        val offset = state.offset
        if (state.str.regionMatches(true, offset, message, 0, message.length)) {
          state.offset += message.length
          ()
        } else {
          state.error = Chain.one(Expectation.Str(offset, message))
          ()
        }
      }
    }

    case class Fail[A]() extends Parser[A] {
      override def parseMut(state: State): A = {
        state.error = Chain.one(Expectation.Fail(state.offset));
        null.asInstanceOf[A]
      }
    }

    case class FailWith[A](message: String) extends Parser[A] {
      override def parseMut(state: State): A = {
        state.error = Chain.one(Expectation.FailWith(state.offset, message));
        null.asInstanceOf[A]
      }
    }

    final def oneOf0[A](all: Array[Parser0[A]], state: State): A = {
      val offset = state.offset
      var errs: Chain[Expectation] = Chain.nil
      var idx = 0
      while (idx < all.length) {
        val thisParser0 = all(idx)
        val res = thisParser0.parseMut(state)
        // we stop if there was no error
        // or if we consumed some input
        val err = state.error
        if ((err eq null) || (state.offset != offset)) {
          return res
        } else {
          // we failed to parse, but didn't consume input
          // is unchanged we continue
          // else we stop
          errs = errs ++ err
          state.error = null
          idx = idx + 1
        }
      }
      // if we got here, all of them failed, but we
      // never advanced the offset
      state.error = errs
      null.asInstanceOf[A]
    }

    case class OneOf[A](all: List[Parser[A]]) extends Parser[A] {
      require(all.lengthCompare(2) >= 0, s"expected more than two items, found: ${all.size}")
      private[this] val ary: Array[Parser0[A]] = all.toArray

      override def parseMut(state: State): A = oneOf0(ary, state)
    }

    case class OneOf0[A](all: List[Parser0[A]]) extends Parser0[A] {
      require(all.lengthCompare(2) >= 0, s"expected more than two items, found: ${all.size}")
      private[this] val ary = all.toArray

      override def parseMut(state: State): A = oneOf0(ary, state)
    }

    final def prod[A, B](pa: Parser0[A], pb: Parser0[B], state: State): (A, B) = {
      val a = pa.parseMut(state)
      if (state.error eq null) {
        val b = pb.parseMut(state)
        if (state.capture && (state.error eq null)) (a, b)
        else null
      } else null
    }

    // we know that at least one of first | second is Parser
    case class Prod[A, B](first: Parser0[A], second: Parser0[B]) extends Parser[(A, B)] {
      require(first.isInstanceOf[Parser[_]] || second.isInstanceOf[Parser[_]])
      override def parseMut(state: State): (A, B) = prod(first, second, state)
    }

    case class Prod0[A, B](first: Parser0[A], second: Parser0[B]) extends Parser0[(A, B)] {
      override def parseMut(state: State): (A, B) = prod(first, second, state)
    }

    final def softProd[A, B](pa: Parser0[A], pb: Parser0[B], state: State): (A, B) = {
      val offset = state.offset
      val a = pa.parseMut(state)
      if (state.error eq null) {
        val offseta = state.offset
        val b = pb.parseMut(state)
        // pa passed, if pb fails without consuming, rewind to offset
        if (state.error ne null) {
          if (state.offset == offseta) {
            state.offset = offset
          }
          // else partial parse of b, don't rewind
          null
        } else if (state.capture) (a, b)
        else null
      } else null
    }

    // we know that at least one of first | second is Parser
    case class SoftProd[A, B](first: Parser0[A], second: Parser0[B]) extends Parser[(A, B)] {
      require(first.isInstanceOf[Parser[_]] || second.isInstanceOf[Parser[_]])
      override def parseMut(state: State): (A, B) = softProd(first, second, state)
    }

    case class SoftProd0[A, B](first: Parser0[A], second: Parser0[B]) extends Parser0[(A, B)] {
      override def parseMut(state: State): (A, B) = softProd(first, second, state)
    }

    final def map[A, B](parser: Parser0[A], fn: A => B, state: State): B = {
      val a = parser.parseMut(state)
      if ((state.error eq null) && state.capture) fn(a)
      else null.asInstanceOf[B]
    }

    case class Map[A, B](parser: Parser0[A], fn: A => B) extends Parser0[B] {
      override def parseMut(state: State): B = Impl.map(parser, fn, state)
    }

    case class Map1[A, B](parser: Parser[A], fn: A => B) extends Parser[B] {
      override def parseMut(state: State): B = Impl.map(parser, fn, state)
    }

    final def select[A, B, C](
        pab: Parser0[Either[A, B]],
        pc: Parser0[C],
        state: State
    ): Either[(A, C), B] = {
      val cap = state.capture
      state.capture = true
      val either = pab.parseMut(state)
      state.capture = cap
      if (state.error eq null)
        either match {
          case Left(a) =>
            val c = pc.parseMut(state)
            if (cap && (state.error eq null)) {
              Left((a, c))
            } else {
              null
            }
          case r @ Right(_) => r.leftCast
        }
      else null
    }

    case class Select0[A, B, C](pab: Parser0[Either[A, B]], pc: Parser0[C])
        extends Parser0[Either[(A, C), B]] {
      override def parseMut(state: State): Either[(A, C), B] =
        Impl.select(pab, pc, state)
    }

    case class Select[A, B, C](pab: Parser[Either[A, B]], pc: Parser0[C])
        extends Parser[Either[(A, C), B]] {
      override def parseMut(state: State): Either[(A, C), B] =
        Impl.select(pab, pc, state)
    }

    final def flatMap[A, B](parser: Parser0[A], fn: A => Parser0[B], state: State): B = {
      // we can't void before flatMap unfortunately, because
      // we need to be able to produce the next parser
      val cap = state.capture
      state.capture = true
      val a = parser.parseMut(state)
      state.capture = cap

      if (state.error eq null) {
        fn(a).parseMut(state)
      } else null.asInstanceOf[B]
    }

    case class FlatMap0[A, B](parser: Parser0[A], fn: A => Parser0[B]) extends Parser0[B] {
      override def parseMut(state: State): B = Impl.flatMap(parser, fn, state)
    }

    // at least one of the parsers needs to be a Parser
    case class FlatMap[A, B](parser: Parser0[A], fn: A => Parser0[B]) extends Parser[B] {
      override def parseMut(state: State): B = Impl.flatMap(parser, fn, state)
    }

    final def tailRecM[A, B](
        init: Parser0[Either[A, B]],
        fn: A => Parser0[Either[A, B]],
        state: State
    ): B = {
      var p: Parser0[Either[A, B]] = init
      // we have to capture
      val c0 = state.capture
      state.capture = true
      while (state.error eq null) {
        val res = p.parseMut(state)
        if (state.error eq null) {
          res match {
            case Right(b) =>
              state.capture = c0
              return b
            case Left(a) =>
              p = fn(a)
          }
        }
      }
      state.capture = c0
      null.asInstanceOf[B]
    }

    case class TailRecM[A, B](init: A, fn: A => Parser0[Either[A, B]]) extends Parser0[B] {
      private[this] val p1 = fn(init)

      override def parseMut(state: State): B = Impl.tailRecM(p1, fn, state)
    }

    case class TailRecM1[A, B](init: A, fn: A => Parser[Either[A, B]]) extends Parser[B] {
      private[this] val p1 = fn(init)

      override def parseMut(state: State): B = Impl.tailRecM(p1, fn, state)
    }

    @annotation.tailrec
    final def compute0[A](fn: () => Parser0[A]): Parser0[A] =
      fn() match {
        case Defer(f) => compute(f)
        case Defer0(f) => compute0(f)
        case notDefer0 => notDefer0
      }
    @annotation.tailrec
    final def compute[A](fn: () => Parser[A]): Parser[A] =
      fn() match {
        case Defer(f) => compute(f)
        case notDefer0 => notDefer0
      }

    case class Defer[A](fn: () => Parser[A]) extends Parser[A] {
      private[this] var computed: Parser0[A] = null
      override def parseMut(state: State): A = {

        val p0 = computed
        val p =
          if (p0 ne null) p0
          else {
            val res = compute(fn)
            computed = res
            res
          }

        p.parseMut(state)
      }
    }

    case class Defer0[A](fn: () => Parser0[A]) extends Parser0[A] {
      private[this] var computed: Parser0[A] = null
      override def parseMut(state: State): A = {

        val p0 = computed
        val p =
          if (p0 ne null) p0
          else {
            val res = compute0(fn)
            computed = res
            res
          }

        p.parseMut(state)
      }
    }

    final def repCapture[A, B](
        p: Parser[A],
        min: Int,
        state: State,
        append: Appender[A, B]
    ): Boolean = {
      var offset = state.offset
      var cnt = 0

      while (true) {
        val a = p.parseMut(state)
        if (state.error eq null) {
          cnt += 1
          append.append(a)
          offset = state.offset
        } else {
          // there has been an error
          if ((state.offset == offset) && (cnt >= min)) {
            // we correctly read at least min items
            // reset the error to make the success
            state.error = null
            return true
          } else {
            // else we did a partial read then failed
            // but didn't read at least min items
            return false
          }
        }
      }
      // $COVERAGE-OFF$
      // unreachable due to infinite loop
      return false
      // $COVERAGE-ON$
    }

    final def repNoCapture[A](p: Parser[A], min: Int, state: State): Unit = {
      var offset = state.offset
      var cnt = 0

      while (true) {
        p.parseMut(state)
        if (state.error eq null) {
          cnt += 1
          offset = state.offset
        } else {
          // there has been an error
          if ((state.offset == offset) && (cnt >= min)) {
            // we correctly read at least min items
            // reset the error to make the success
            state.error = null
          }
          // else we did a partial read then failed
          // but didn't read at least min items
          return ()
        }
      }
    }

    case class Rep0[A, B](p1: Parser[A], acc: Accumulator0[A, B]) extends Parser0[B] {
      private[this] val ignore: B = null.asInstanceOf[B]

      override def parseMut(state: State): B = {
        if (state.capture) {
          val app = acc.newAppender()
          if (repCapture(p1, 0, state, app)) app.finish()
          else ignore
        } else {
          repNoCapture(p1, 0, state)
          ignore
        }
      }
    }

    case class Rep[A, B](p1: Parser[A], min: Int, acc1: Accumulator[A, B]) extends Parser[B] {
      if (min < 1) throw new IllegalArgumentException(s"expected min >= 1, found: $min")

      private[this] val ignore: B = null.asInstanceOf[B]

      override def parseMut(state: State): B = {
        val head = p1.parseMut(state)

        if (state.error ne null) ignore
        else if (state.capture) {
          val app = acc1.newAppender(head)
          if (repCapture(p1, min - 1, state, app)) app.finish()
          else ignore
        } else {
          repNoCapture(p1, min - 1, state)
          ignore
        }
      }
    }

    // invariant: input must be sorted
    def rangesFor(charArray: Array[Char]): NonEmptyList[(Char, Char)] = {
      def rangesFrom(start: Char, end: Char, idx: Int): NonEmptyList[(Char, Char)] =
        if (idx >= charArray.length || (idx < 0)) NonEmptyList((start, end), Nil)
        else {
          val end1 = charArray(idx)
          if ((end1.toInt == end.toInt + 1) || (end1 == end)) rangesFrom(start, end1, idx + 1)
          else {
            // we had a break:
            (start, end) :: rangesFrom(end1, end1, idx + 1)
          }
        }

      rangesFrom(charArray(0), charArray(0), 1)
    }

    /*
     * Merge CharIn bitsets
     */
    def mergeCharIn[A, P0 <: Parser0[A]](ps: List[P0]): List[P0] = {
      @annotation.tailrec
      def loop(ps: List[P0], front: List[(Int, BitSetUtil.Tpe)], result: Chain[P0]): Chain[P0] = {
        @inline
        def frontRes: Chain[P0] =
          if (front.isEmpty) Chain.nil
          else Chain.one(Parser0.charIn(BitSetUtil.union(front)).asInstanceOf[P0])

        ps match {
          case Nil => result ++ frontRes
          case AnyChar :: tail =>
            // AnyChar is bigger than all subsequent CharIn:
            // and any direct prefix CharIns
            val tail1 = tail.filterNot(_.isInstanceOf[CharIn])
            (result :+ AnyChar.asInstanceOf[P0]) ++ Chain.fromSeq(tail1)
          case CharIn(m, bs, _) :: tail =>
            loop(tail, (m, bs) :: front, result)
          case h :: tail =>
            // h is not an AnyChar or CharIn
            // we make our prefix frontRes
            // and resume working on the tail
            loop(tail, Nil, (result ++ frontRes) :+ h)
        }
      }

      loop(ps, Nil, Chain.nil).toList
    }

    case object AnyChar extends Parser[Char] {
      override def parseMut(state: State): Char = {
        val offset = state.offset
        if (offset < state.str.length) {
          val char = state.str.charAt(offset)
          state.offset += 1
          char
        } else {
          state.error = Chain.one(Expectation.InRange(offset, Char.MinValue, Char.MaxValue))
          '\u0000'
        }
      }
    }

    case class CharIn(min: Int, bitSet: BitSetUtil.Tpe, ranges: NonEmptyList[(Char, Char)])
        extends Parser[Char] {

      override def toString = s"CharIn($min, bitSet = ..., $ranges)"

      def makeError(offset: Int): Chain[Expectation] = {
        var result = Chain.empty[Expectation]
        var aux = ranges.toList
        while (aux.nonEmpty) {
          val (s, e) = aux.head
          result = result :+ Expectation.InRange(offset, s, e)
          aux = aux.tail
        }
        result
      }

      override def parseMut(state: State): Char = {
        val offset = state.offset
        if (offset < state.str.length) {
          val char = state.str.charAt(offset)
          val cInt = char.toInt
          if (BitSetUtil.isSet(bitSet, cInt - min)) {
            // we found the character
            state.offset = offset + 1
            char
          } else {
            state.error = makeError(offset)
            '\u0000'
          }
        } else {
          state.error = makeError(offset)
          '\u0000'
        }
      }
    }

    /*
     * If pa fails, succeed parsing nothing
     * else fail
     */
    case class Not(under: Parser0[Unit]) extends Parser0[Unit] {
      override def parseMut(state: State): Unit = {
        val offset = state.offset
        under.parseMut(state)
        if (state.error ne null) {
          // under failed, so we succeed
          state.error = null
        } else {
          // under succeeded but we expected failure here
          val matchedStr = state.str.substring(offset, state.offset)
          // we don't reset the offset, so if the underlying parser
          // advanced it will fail in a OneOf
          state.error = Chain.one(Expectation.ExpectedFailureAt(offset, matchedStr))
        }

        state.offset = offset
        ()
      }
    }

    /*
     * succeeds if the underlying parser succeeds, but we do
     * not advance
     */
    case class Peek(under: Parser0[Unit]) extends Parser0[Unit] {
      override def parseMut(state: State): Unit = {
        val offset = state.offset
        under.parseMut(state)
        if (state.error eq null) {
          // under passed, so we succeed
          state.offset = offset
        }
        // else under failed, so we fail
        ()
      }
    }
  }
}

abstract class ParserInstances {
  implicit val catInstancesParser0
      : Monad[Parser0] with Alternative[Parser0] with Defer[Parser0] with FunctorFilter[Parser0] =
    new Monad[Parser0] with Alternative[Parser0] with Defer[Parser0] with FunctorFilter[Parser0] {
      def pure[A](a: A): Parser0[A] = Parser0.pure(a)

      def defer[A](a: => Parser0[A]) = Parser0.defer0(a)

      def empty[A]: Parser0[A] = Parser0.Fail

      def functor = this

      override def map[A, B](fa: Parser0[A])(fn: A => B): Parser0[B] = Parser0.map0(fa)(fn)

      def mapFilter[A, B](fa: Parser0[A])(f: A => Option[B]): Parser0[B] =
        fa.mapFilter(f)

      override def filter[A](fa: Parser0[A])(fn: A => Boolean): Parser0[A] =
        fa.filter(fn)

      override def filterNot[A](fa: Parser0[A])(fn: A => Boolean): Parser0[A] =
        fa.filter { a => !fn(a) }

      override def product[A, B](fa: Parser0[A], fb: Parser0[B]): Parser0[(A, B)] =
        Parser0.product0(fa, fb)

      override def map2[A, B, C](pa: Parser0[A], pb: Parser0[B])(fn: (A, B) => C): Parser0[C] =
        map(product(pa, pb)) { case (a, b) => fn(a, b) }

      override def map2Eval[A, B, C](pa: Parser0[A], pb: Eval[Parser0[B]])(
          fn: (A, B) => C
      ): Eval[Parser0[C]] =
        Now(pb match {
          case Now(pb) => map2(pa, pb)(fn)
          case later => map2(pa, defer(later.value))(fn)
        })

      override def ap[A, B](pf: Parser0[A => B])(pa: Parser0[A]): Parser0[B] =
        map(product(pf, pa)) { case (fn, a) => fn(a) }

      def flatMap[A, B](fa: Parser0[A])(fn: A => Parser0[B]): Parser0[B] =
        Parser0.flatMap0(fa)(fn)

      def combineK[A](pa: Parser0[A], pb: Parser0[A]): Parser0[A] =
        Parser0.oneOf0(pa :: pb :: Nil)

      def tailRecM[A, B](init: A)(fn: A => Parser0[Either[A, B]]): Parser0[B] =
        Parser0.tailRecM(init)(fn)

      override def void[A](pa: Parser0[A]): Parser0[Unit] =
        Parser0.void0(pa)

      override def as[A, B](pa: Parser0[A], b: B): Parser0[B] =
        Parser0.as0(pa, b)

      override def productL[A, B](pa: Parser0[A])(pb: Parser0[B]): Parser0[A] =
        map(product(pa, pb.void)) { case (a, _) => a }

      override def productR[A, B](pa: Parser0[A])(pb: Parser0[B]): Parser0[B] =
        map(product(pa.void, pb)) { case (_, b) => b }

      override def productLEval[A, B](fa: Parser0[A])(fb: Eval[Parser0[B]]): Parser0[A] = {
        val pb =
          fb match {
            case Now(pb) => pb
            case notNow => defer(notNow.value)
          }

        productL(fa)(pb)
      }

      override def productREval[A, B](fa: Parser0[A])(fb: Eval[Parser0[B]]): Parser0[B] = {
        val pb =
          fb match {
            case Now(pb) => pb
            case notNow => defer(notNow.value)
          }

        productR(fa)(pb)
      }

    }
}
