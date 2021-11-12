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

import cats.{Eval, FunctorFilter, Monad, Defer, Alternative, FlatMap, Now, MonoidK, Order}
import cats.data.{AndThen, Chain, NonEmptyList}

import cats.implicits._
import scala.collection.immutable.SortedSet
import scala.collection.mutable.ListBuffer
import java.util.Arrays

/** Parser0[A] attempts to extract an `A` value from the given input, potentially moving its offset
  * forward in the process.
  *
  * When calling `parse`, one of three outcomes occurs:
  *
  *   - Success: The parser consumes zero-or-more characters of input and successfully extracts a
  *     value. The input offset will be moved forward by the number of characters consumed.
  *
  *   - Epsilon failure: The parser fails to extract a value without consuming any characters of
  *     input. The input offset will not be changed.
  *
  *   - Arresting failure: The parser fails to extract a value but does consume one-or-more
  *     characters of input. The input offset will be moved forward by the number of characters
  *     consumed and all parsing will stop (unless a higher-level parser backtracks).
  *
  * Operations such as `x.orElse(y)` will only consider parser `y` if `x` returns an epsilon
  * failure; these methods cannot recover from an arresting failure. Arresting failures can be
  * "rewound" using methods such as `x.backtrack` (which converts arresting failures from `x` into
  * epsilon failures), or `softProduct(x, y)` (which can rewind successful parses by `x` that are
  * followed by epsilon failures for `y`).
  *
  * Rewinding tends to make error reporting more difficult and can lead to exponential parser
  * behavior it is not the default behavior.
  */
sealed abstract class Parser0[+A] { self: Product =>

  /** Attempt to parse an `A` value out of `str`.
    *
    * This method will either return a failure, or else the remaining string and the parsed value.
    *
    * To require the entire input to be consumed, see `parseAll`.
    */
  final def parse(str: String): Either[Parser.Error, (String, A)] = {
    val state = new Parser.State(str)
    val result = parseMut(state)
    val err = state.error
    val offset = state.offset
    if (err eq null) Right((str.substring(offset), result))
    else
      Left(
        Parser.Error(
          offset,
          Parser.Expectation.unify(NonEmptyList.fromListUnsafe(err.value.toList))
        )
      )
  }

  /** Attempt to parse all of the input `str` into an `A` value.
    *
    * This method will return a failure unless all of `str` is consumed during parsing.
    *
    * `p.parseAll(s)` is equivalent to `(p <* Parser.end).parse(s).map(_._2)`.
    */
  final def parseAll(str: String): Either[Parser.Error, A] = {
    val state = new Parser.State(str)
    val result = parseMut(state)
    val err = state.error
    val offset = state.offset
    if (err eq null) {
      if (offset == str.length) Right(result)
      else
        Left(
          Parser.Error(
            offset,
            NonEmptyList(Parser.Expectation.EndOfString(offset, str.length), Nil)
          )
        )
    } else
      Left(
        Parser.Error(
          offset,
          Parser.Expectation.unify(NonEmptyList.fromListUnsafe(err.value.toList))
        )
      )
  }

  /** Convert epsilon failures into None values.
    *
    * Normally if a parser fails to consume any input it fails with an epsilon failure. The `?`
    * method converts these failures into None values (and wraps other values in `Some(_)`).
    *
    * If the underlying parser failed with other errors, this parser will still fail.
    */
  def ? : Parser0[Option[A]] =
    Parser.oneOf0(Parser.map0(this)(Some(_)) :: Parser.optTail)

  /** If this parser fails to parse its input with an epsilon error, try the given parser instead.
    *
    * If this parser fails with an arresting error, the next parser won't be tried.
    *
    * Backtracking may be used on the left parser to allow the right one to pick up after any error,
    * resetting any state that was modified by the left parser.
    *
    * This method is similar to Parser#orElse but returns Either.
    */
  def eitherOr[B](pb: Parser0[B]): Parser0[Either[B, A]] =
    Parser.eitherOr0(this, pb)

  /** Parse without capturing values.
    *
    * Calling `void` on a parser can be a significant optimization -- it allows the parser to avoid
    * allocating results to return.
    *
    * Other methods like `as`, `*>`, and `<*` use `void` internally to discard allocations, since
    * they will ignore the original parsed result.
    */
  def void: Parser0[Unit] =
    Parser.void0(this)

  /** Return the string matched by this parser.
    *
    * When parsing an input string that the underlying parser matches, this parser will return the
    * matched substring instead of any value that the underlying parser would have returned. It will
    * still match exactly the same inputs as the original parser.
    *
    * This method is very efficient: similarly to `void`, we can avoid allocating results to return.
    */
  def string: Parser0[String] =
    Parser.string0(this)

  /** If this parser fails to match, rewind the offset to the starting point before moving on to
    * other parser.
    *
    * This method converts arresting failures into epsilon failures, which includes rewinding the
    * offset to that used before parsing began.
    *
    * This method will most often be used before calling methods such as `orElse`, `~`, or `flatMap`
    * which involve a subsequent parser picking up where this one left off.
    */
  def backtrack: Parser0[A] =
    Parser.backtrack0(this)

  /** Sequence another parser after this one, combining both results into a tuple.
    *
    * This combinator returns a product of parsers. If this parser successfully produces an `A`
    * value, the other parser is run on the remaining input to try to produce a `B` value.
    *
    * If either parser produces an error the result is an error. Otherwise both extracted values are
    * combined into a tuple.
    */
  def ~[B](that: Parser0[B]): Parser0[(A, B)] =
    Parser.product0(this, that)

  /** Compose two parsers, ignoring the values extracted by the left-hand parser.
    *
    * `x *> y` is equivalent to `(x.void ~ y).map(_._2)`.
    */
  def *>[B](that: Parser0[B]): Parser0[B] =
    (void ~ that).map(_._2)

  /** Compose two parsers, ignoring the values extracted by the right-hand parser.
    *
    * `x <* y` is equivalent to `(x ~ y.void).map(_._1)`.
    */
  def <*[B](that: Parser0[B]): Parser0[A] =
    (this ~ that.void).map(_._1)

  /** If this parser fails to parse its input with an epsilon error, try the given parser instead.
    *
    * If this parser fails with an arresting error, the next parser won't be tried.
    *
    * Backtracking may be used on the left parser to allow the right one to pick up after any error,
    * resetting any state that was modified by the left parser.
    */
  def orElse[A1 >: A](that: Parser0[A1]): Parser0[A1] =
    Parser.oneOf0(this :: that :: Nil)

  /** Synonym for orElse Note this is not commutative: if this has an arresting failure we do not
    * continue onto the next.
    */
  def |[A1 >: A](that: Parser0[A1]): Parser0[A1] =
    orElse(that)

  /** Transform parsed values using the given function.
    *
    * This parser will match the same inputs as the underlying parser, using the given function `f`
    * to transform the values the underlying parser produces.
    *
    * If the underlying value is ignored (e.g. `map(_ => ...)`) calling `void` before `map` will
    * improve the efficiency of the parser.
    */
  def map[B](fn: A => B): Parser0[B] =
    Parser.map0(this)(fn)

  /** Transform parsed values using the given function, or fail on None
    *
    * When the function return None, this parser fails This is implemented with select, which makes
    * it more efficient than using flatMap
    */
  def mapFilter[B](fn: A => Option[B]): Parser0[B] = {
    val leftUnit = Left(())

    val first = map { a =>
      fn(a) match {
        case Some(b) => Right(b)
        case None => leftUnit
      }
    }
    Parser.select0(first)(Parser.Fail)
  }

  /** Transform parsed values using the given function, or fail when not defined
    *
    * When the function is not defined, this parser fails This is implemented with select, which
    * makes it more efficient than using flatMap
    */
  def collect[B](fn: PartialFunction[A, B]): Parser0[B] =
    mapFilter(fn.lift)

  /** If the predicate is not true, fail you may want .filter(fn).backtrack so if the filter fn
    * fails you can fall through in an oneOf0 or orElse
    *
    * Without the backtrack, a failure of the function will be an arresting failure.
    */
  def filter(fn: A => Boolean): Parser0[A] = {
    val leftUnit = Left(())
    Parser.select0(this.map { a =>
      if (fn(a)) Right(a)
      else leftUnit
    })(Parser.Fail)
  }

  /** Dynamically construct the next parser based on the previously parsed value.
    *
    * Using `flatMap` is very expensive. When possible, you should prefer to use methods such as
    * `~`, `*>`, or `<*` when possible, since these are much more efficient.
    */
  def flatMap[B](fn: A => Parser0[B]): Parser0[B] =
    Parser.flatMap0(this)(fn)

  /** Replaces parsed values with the given value.
    */
  def as[B](b: B): Parser0[B] =
    Parser.as0(this, b)

  /** Wrap this parser in a helper class, enabling better composition with `Parser` values.
    *
    * For example, with `p: Parser0[Int]` and `p1: Parser0[Double]`:
    *
    * val a1: Parser0[(Int, Double)] = p ~ p1 val a2: Parser[(Int, Double)] = p.with1 ~ p1
    *
    * val b1: Parser0[Double] = p *> p1 val b2: Parser[Double] = p.with1 *> p1
    *
    * val c1: Parser0[Int] = p <* p1 val c2: Parser[Int] = p.with1 <* p1
    *
    * Without using `with1`, these methods will return `Parser0` values since they are not known to
    * return `Parser` values instead.
    */
  def with1: Parser.With1[A] =
    new Parser.With1(this)

  /** Wrap this parser in a helper class, to enable backtracking during composition.
    *
    * This wrapper changes the behavior of `~`, `<*` and `*>`. Normally no backtracking occurs.
    * Using `soft` on the left-hand side will enable backtracking if the right-hand side returns an
    * epsilon failure (but not in any other case).
    *
    * For example, `(x ~ y)` will never backtrack. But with `(x.soft ~ y)`, if `x` parses
    * successfully, and `y` returns an epsilon failure, the parser will "rewind" to the point before
    * `x` began.
    */
  def soft: Parser.Soft0[A] =
    new Parser.Soft0(this)

  /** Return a parser that succeeds (consuming nothing, and extracting nothing) if the current
    * parser would fail.
    *
    * This parser expects the underlying parser to fail, and will unconditionally backtrack after
    * running it.
    */
  def unary_! : Parser0[Unit] =
    Parser.not(this)

  /** Return a parser that succeeds (consuming nothing and extracting nothing) if the current parser
    * would also succeed.
    *
    * This parser expects the underlying parser to succeed, and will unconditionally backtrack after
    * running it.
    */
  def peek: Parser0[Unit] =
    Parser.peek(this)

  /** Use this parser to parse between values.
    *
    * Parses `b` followed by `this` and `c`. Returns only the values extracted by `this` parser.
    */
  def between(b: Parser0[Any], c: Parser0[Any]): Parser0[A] =
    (b.void ~ (this ~ c.void)).map { case (_, (a, _)) => a }

  /** Use this parser to parse surrounded by values.
    *
    * This is the same as `between(b, b)`
    */
  def surroundedBy(b: Parser0[Any]): Parser0[A] =
    between(b, b)

  /** Add a string context to any Errors on parsing this is useful for debugging failing parsers.
    */
  def withContext(str: String): Parser0[A] =
    Parser.withContext0(this, str)

  /** Internal (mutable) parsing method.
    *
    * This method should only be called internally by parser instances.
    */
  private[parse] def parseMut(state: Parser.State): A

  /** This method overrides `Object#hashCode` to cache its result for performance reasons.
    */
  override lazy val hashCode: Int = scala.runtime.ScalaRunTime._hashCode(this)
}

/** Parser[A] is a Parser0[A] that will always consume one-or-more characters on a successful parse.
  *
  * Since Parser is guaranteed to consume input it provides additional methods which would be unsafe
  * when used on parsers that succeed without consuming input, such as `rep0`.
  *
  * When a Parser is composed with a Parser0 the result is usually a Parser. Parser overrides many
  * of Parser0's methods to refine the return type. In other cases, callers may need to use the
  * `with1` helper method to refine the type of their expressions.
  *
  * Parser doesn't provide any additional guarantees over Parser0 on what kind of parsing failures
  * it can return.
  */
sealed abstract class Parser[+A] extends Parser0[A] { self: Product =>

  /** This method overrides `Parser0#filter` to refine the return type.
    */
  override def filter(fn: A => Boolean): Parser[A] = {
    val leftUnit = Left(())
    Parser.select(this.map { a =>
      if (fn(a)) Right(a)
      else leftUnit
    })(Parser.Fail)
  }

  /** This method overrides `Parser0#void` to refine the return type.
    */
  override def void: Parser[Unit] =
    Parser.void(this)

  /** This method overrides `Parser0#string` to refine the return type.
    */
  override def string: Parser[String] =
    Parser.string(this)

  /** This method overrides `Parser0#backtrack` to refine the return type.
    */
  override def backtrack: Parser[A] =
    Parser.backtrack(this)

  /** a version of eitherOr when both sides are not Parser0
    */
  def eitherOr[B](pb: Parser[B]): Parser[Either[B, A]] =
    Parser.eitherOr(this, pb)

  /** This method overrides `Parser0#~` to refine the return type.
    */
  override def ~[B](that: Parser0[B]): Parser[(A, B)] =
    Parser.product10(this, that)

  /** This method overrides `Parser0#*>` to refine the return type.
    */
  override def *>[B](that: Parser0[B]): Parser[B] =
    (void ~ that).map(_._2)

  /** This method overrides `Parser0#<*` to refine the return type.
    */
  override def <*[B](that: Parser0[B]): Parser[A] =
    (this ~ that.void).map(_._1)

  /** This method overrides `Parser0#collect` to refine the return type.
    */
  override def collect[B](fn: PartialFunction[A, B]): Parser[B] =
    mapFilter(fn.lift)

  /** This method overrides `Parser0#map` to refine the return type.
    */
  override def map[B](fn: A => B): Parser[B] =
    Parser.map(this)(fn)

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
    Parser.select(first)(Parser.Fail)
  }

  /** This method overrides `Parser0#flatMap` to refine the return type.
    */
  override def flatMap[B](fn: A => Parser0[B]): Parser[B] =
    Parser.flatMap10(this)(fn)

  /** This method overrides `Parser0#as` to refine the return type.
    */
  override def as[B](b: B): Parser[B] =
    Parser.as(this, b)

  /** If this parser fails to parse its input with an epsilon error, try the given parser instead.
    *
    * This method is similar to Parser0#orElse, but since both arguments are known to be Parser
    * values, the result is known to be a Parser as well.
    */
  def orElse[A1 >: A](that: Parser[A1]): Parser[A1] =
    Parser.oneOf(this :: that :: Nil)

  /** Synonym for orElse Note this is not commutative: if this has an arresting failure we do not
    * continue onto the next.
    */
  def |[A1 >: A](that: Parser[A1]): Parser[A1] =
    orElse(that)

  /** Use this parser to parse zero-or-more values.
    *
    * This parser may succeed without consuming input in the case where zero values are parsed.
    *
    * If the underlying parser hits an arresting failure, the entire parse is also an arresting
    * failure. If the underlying parser hits an epsilon failure, the parsed values (if any) are
    * returned in a list as a successful parse.
    */
  def rep0: Parser0[List[A]] = repAs0

  /** Use this parser to parse at least `min` values (where `min >= 0`).
    *
    * If `min` is zero, this parser may succeed without consuming input in the case where zero
    * values are parsed. If `min` is known to be greater than zero, consider using `rep(min)`
    * instead.
    *
    * Like `rep0`, arresting failures in the underlying parser will result in an arresting failure.
    * Unlike `rep0`, this method may also return an arresting failure if it has not parsed at least
    * `min` values (but has consumed input).
    */
  def rep0(min: Int): Parser0[List[A]] =
    if (min == 0) rep0
    else repAs(min)

  /** Repeat the parser `min` or more times, but no more than `max`
    *
    * The parser fails if it can't match at least `min` times After repeating the parser `max`
    * times, the parser completes successfully
    *
    * @throws java.lang.IllegalArgumentException
    *   if min < 0 or max < min
    */
  def rep0(min: Int, max: Int): Parser0[List[A]] =
    if (min == 0) repAs0(max)
    else repAs(min, max)

  /** Use this parser to parse one-or-more values.
    *
    * This parser behaves like `rep0`, except that it must produce at least one value, and is
    * guaranteed to consume input on successful parses.
    */
  def rep: Parser[NonEmptyList[A]] = repAs

  /** Use this parser to parse at least `min` values (where `min >= 1`).
    *
    * This method behaves likes `rep`, except that if fewer than `min` values are produced an
    * arresting failure will be returned.
    */
  def rep(min: Int): Parser[NonEmptyList[A]] =
    repAs(min = min)

  /** Repeat the parser `min` or more times, but no more than `max`
    *
    * The parser fails if it can't match at least `min` times After repeating the parser `max`
    * times, the parser completes successfully
    *
    * @throws java.lang.IllegalArgumentException
    *   if min < 1 or max < min
    */
  def rep(min: Int, max: Int): Parser[NonEmptyList[A]] =
    repAs(min = min, max = max)

  /** Repeat the parser 0 or more times
    *
    * @note
    *   this can wind up parsing nothing
    */
  def repAs0[B](implicit acc: Accumulator0[A, B]): Parser0[B] =
    Parser.repAs0(this)(acc)

  /** Repeat the parser 0 or more times, but no more than `max`
    *
    * It may seem weird to accept 0 here, but without, composing this method becomes more complex.
    * Since and empty parse is possible for this method, we do allow max = 0
    *
    * @throws java.lang.IllegalArgumentException
    *   if max < 0
    *
    * @note
    *   this can wind up parsing nothing
    */
  def repAs0[B](max: Int)(implicit acc: Accumulator0[A, B]): Parser0[B] =
    Parser.repAs0(this, max = max)(acc)

  /** Repeat the parser 1 or more times
    */
  def repAs[B](implicit acc: Accumulator[A, B]): Parser[B] =
    repAs(min = 1)

  /** Repeat the parser `min` or more times
    *
    * The parser fails if it can't match at least `min` times
    *
    * @throws java.lang.IllegalArgumentException
    *   if min < 1
    */
  def repAs[B](min: Int)(implicit acc: Accumulator[A, B]): Parser[B] =
    Parser.repAs(this, min = min)(acc)

  /** Repeat the parser `min` or more times, but no more than `max`
    *
    * The parser fails if it can't match at least `min` times After repeating the parser `max`
    * times, the parser completes successfully
    *
    * @throws java.lang.IllegalArgumentException
    *   if min < 1 or max < min
    */
  def repAs[B](min: Int, max: Int)(implicit acc: Accumulator[A, B]): Parser[B] =
    Parser.repAs(this, min = min, max = max)(acc)

  /** Repeat the parser exactly `times` times
    *
    * @throws java.lang.IllegalArgumentException
    *   if times < 1
    */
  def repExactlyAs[B](times: Int)(implicit acc: Accumulator[A, B]): Parser[B] =
    Parser.repExactlyAs(this, times = times)(acc)

  /** Repeat 0 or more times with a separator
    */
  def repSep0(sep: Parser0[Any]): Parser0[List[A]] =
    Parser.repSep0(this, sep)

  /** Repeat `min` or more times with a separator.
    *
    * @throws java.lang.IllegalArgumentException
    *   if `min < 0`
    */
  def repSep0(min: Int, sep: Parser0[Any]): Parser0[List[A]] =
    Parser.repSep0(this, min = min, sep = sep)

  /** Repeat `min` or more, up to `max` times with a separator.
    *
    * @throws java.lang.IllegalArgumentException
    *   if `min < 0` or `max < min`
    */
  def repSep0(min: Int, max: Int, sep: Parser0[Any]): Parser0[List[A]] =
    Parser.repSep0(this, min = min, max = max, sep = sep)

  /** Repeat 1 or more times with a separator
    */
  def repSep(sep: Parser0[Any]): Parser[NonEmptyList[A]] =
    Parser.repSep(this, sep)

  /** Repeat `min` or more times with a separator, at least once.
    *
    * @throws java.lang.IllegalArgumentException
    *   if `min <= 0`
    */
  def repSep(min: Int, sep: Parser0[Any]): Parser[NonEmptyList[A]] =
    Parser.repSep(this, min = min, sep = sep)

  /** Repeat `min` or more, up to `max` times with a separator, at least once.
    *
    * @throws java.lang.IllegalArgumentException
    *   if `min <= 0` or `max < min`
    */
  def repSep(min: Int, max: Int, sep: Parser0[Any]): Parser[NonEmptyList[A]] =
    Parser.repSep(this, min = min, max = max, sep = sep)

  /** Repeat this parser 0 or more times until `end` Parser succeeds.
    */
  def repUntil0(end: Parser0[Any]): Parser0[List[A]] =
    Parser.repUntil0(this, end)

  /** Repeat this parser 1 or more times until `end` Parser succeeds.
    */
  def repUntil(end: Parser0[Any]): Parser[NonEmptyList[A]] =
    Parser.repUntil(this, end)

  /** Repeat this parser 0 or more times until `end` Parser succeeds.
    */
  def repUntilAs0[B](end: Parser0[Any])(implicit acc: Accumulator0[A, B]): Parser0[B] =
    Parser.repUntilAs0(this, end)

  /** Repeat this parser 1 or more times until `end` Parser succeeds.
    */
  def repUntilAs[B](end: Parser0[Any])(implicit acc: Accumulator[A, B]): Parser[B] =
    Parser.repUntilAs(this, end)

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
  override def soft: Parser.Soft[A] =
    new Parser.Soft(this)

  /** This method overrides `Parser0#withContext` to refine the return type. add a string context to
    * any Errors on parsing this is useful for debugging failing parsers.
    */
  override def withContext(str: String): Parser[A] =
    Parser.withContext(this, str)
}

object Parser {

  /** An expectation reports the kind or parsing error and where it occured.
    */
  sealed abstract class Expectation {
    def offset: Int

    /** This is a reverse order stack (most recent context first) of this parsing error
      */
    def context: List[String] =
      this match {
        case Expectation.WithContext(ctx, inner) =>
          ctx :: inner.context
        case _ => Nil
      }
  }

  object Expectation {
    case class OneOfStr(offset: Int, strs: List[String]) extends Expectation
    // expected a character in a given range
    case class InRange(offset: Int, lower: Char, upper: Char) extends Expectation
    case class StartOfString(offset: Int) extends Expectation
    case class EndOfString(offset: Int, length: Int) extends Expectation
    case class Length(offset: Int, expected: Int, actual: Int) extends Expectation
    case class ExpectedFailureAt(offset: Int, matched: String) extends Expectation
    // this is the result of oneOf0(Nil) at a given location
    case class Fail(offset: Int) extends Expectation
    case class FailWith(offset: Int, message: String) extends Expectation
    case class WithContext(contextStr: String, expect: Expectation) extends Expectation {
      def offset: Int = expect.offset
    }

    implicit val catsOrderExpectation: Order[Expectation] =
      new Order[Expectation] {
        override def compare(left: Expectation, right: Expectation): Int = {
          val c = Integer.compare(left.offset, right.offset)
          if (c != 0) c
          else if (left == right) 0
          else {
            // these are never equal
            (left, right) match {
              case (OneOfStr(_, s1), OneOfStr(_, s2)) => s1.compare(s2)
              case (OneOfStr(_, _), _) => -1
              case (InRange(_, _, _), OneOfStr(_, _)) => 1
              case (InRange(_, l1, u1), InRange(_, l2, u2)) =>
                val c1 = Character.compare(l1, l2)
                if (c1 == 0) Character.compare(u1, u2)
                else c1
              case (InRange(_, _, _), _) => -1
              case (StartOfString(_), OneOfStr(_, _) | InRange(_, _, _)) => 1
              case (StartOfString(_), _) =>
                -1 // if they have the same offset, already handled above
              case (
                    EndOfString(_, _),
                    OneOfStr(_, _) | InRange(_, _, _) | StartOfString(_)
                  ) =>
                1
              case (EndOfString(_, l1), EndOfString(_, l2)) =>
                Integer.compare(l1, l2)
              case (EndOfString(_, _), _) => -1
              case (
                    Length(_, _, _),
                    OneOfStr(_, _) | InRange(_, _, _) | StartOfString(_) | EndOfString(_, _)
                  ) =>
                1
              case (Length(_, e1, a1), Length(_, e2, a2)) =>
                val c1 = Integer.compare(e1, e2)
                if (c1 == 0) Integer.compare(a1, a2)
                else c1
              case (Length(_, _, _), _) => -1
              case (ExpectedFailureAt(_, _), Fail(_) | FailWith(_, _) | WithContext(_, _)) => -1
              case (ExpectedFailureAt(_, m1), ExpectedFailureAt(_, m2)) =>
                m1.compare(m2)
              case (ExpectedFailureAt(_, _), _) => 1
              case (Fail(_), FailWith(_, _) | WithContext(_, _)) => -1
              case (Fail(_), _) => 1
              case (FailWith(_, _), WithContext(_, _)) => -1
              case (FailWith(_, s1), FailWith(_, s2)) =>
                s1.compare(s2)
              case (FailWith(_, _), _) => 1
              case (WithContext(lctx, lexp), WithContext(rctx, rexp)) =>
                val c = compare(lexp, rexp)
                if (c != 0) c
                else lctx.compareTo(rctx)
              case (WithContext(_, _), _) => 1
            }
          }
        }
      }

    private def mergeInRange(irs: List[InRange]): List[InRange] = {
      @annotation.tailrec
      def merge(rs: List[InRange], aux: Chain[InRange] = Chain.empty): Chain[InRange] =
        rs match {
          case x :: y :: rest =>
            if (y.lower.toInt > x.upper.toInt + 1) merge(y :: rest, aux :+ x)
            else merge(InRange(x.offset, x.lower, x.upper max y.upper) :: rest, aux)
          case _ =>
            aux ++ Chain.fromSeq(rs.reverse)
        }
      merge(irs.sortBy(_.lower)).toList
    }

    private def mergeOneOfStr(ooss: List[OneOfStr]): Option[OneOfStr] =
      if (ooss.isEmpty) None
      else {
        val ssb = SortedSet.newBuilder[String]
        ooss.foreach(ssb ++= _.strs)
        Some(OneOfStr(ooss.head.offset, ssb.result().toList))
      }

    @annotation.tailrec
    private def stripContext(ex: Expectation): Expectation =
      ex match {
        case WithContext(_, inner) => stripContext(inner)
        case _ => ex
      }

    @annotation.tailrec
    private def addContext(revCtx: List[String], ex: Expectation): Expectation =
      revCtx match {
        case Nil => ex
        case h :: tail => addContext(tail, WithContext(h, ex))
      }

    /** Sort, dedup and unify ranges for the errors accumulated This is called just before finally
      * returning an error in Parser.parse
      */
    def unify(errors: NonEmptyList[Expectation]): NonEmptyList[Expectation] = {
      val result = errors
        .groupBy { ex => (ex.offset, ex.context) }
        .iterator
        .flatMap { case ((_, ctx), list) =>
          val rm = ListBuffer.empty[InRange]
          val om = ListBuffer.empty[OneOfStr]
          val fails = ListBuffer.empty[Fail]
          val others = ListBuffer.empty[Expectation]

          var items = list.toList
          while (items.nonEmpty) {
            stripContext(items.head) match {
              case ir: InRange => rm += ir
              case os: OneOfStr => om += os
              case fail: Fail => fails += fail
              case other => others += other
            }
            items = items.tail
          }

          // merge all the ranges:
          val rangeMerge = mergeInRange(rm.toList)
          // merge the OneOfStr
          val oossMerge = mergeOneOfStr(om.toList)

          val errors = others.toList reverse_::: (oossMerge ++: rangeMerge)
          val finals = if (errors.isEmpty) fails.toList else errors
          if (ctx.nonEmpty) {
            val revCtx = ctx.reverse
            finals.map(addContext(revCtx, _))
          } else finals
        }
        .toList

      NonEmptyList.fromListUnsafe(result.distinct.sorted)
    }
  }

  /** Represents where a failure occurred and all the expectations that were broken
    */
  final case class Error(failedAtOffset: Int, expected: NonEmptyList[Expectation]) {
    def offsets: NonEmptyList[Int] =
      expected.map(_.offset).distinct
  }

  /** Enables syntax to access product01, product and flatMap01 This helps us build Parser instances
    * when starting from a Parser0
    */
  final class With1[+A](val parser: Parser0[A]) extends AnyVal {

    /** parser then that. Since that is a Parser the result is
      */
    def ~[B](that: Parser[B]): Parser[(A, B)] =
      Parser.product01(parser, that)

    /** This is the usual monadic composition, but you should much prefer to use ~ or Apply.product,
      * *>, <*, etc if you can since it is much more efficient. This has to call fn on each parse,
      * which could be a lot of extra work is you already know the result as is the case for ~
      */
    def flatMap[B](fn: A => Parser[B]): Parser[B] =
      Parser.flatMap01(parser)(fn)

    /** parser then that. Since that is a Parser the result is
      */
    def *>[B](that: Parser[B]): Parser[B] =
      product01(void0(parser), that).map(_._2)

    /** parser then that. Since that is a Parser the result is
      */
    def <*[B](that: Parser[B]): Parser[A] =
      product01(parser, void(that)).map(_._1)

    /** If we can parse this then that, do so, if we fail that without consuming, rewind before this
      * without consuming. If either consume 1 or more, do not rewind
      */
    def soft: Soft01[A] =
      new Soft01(parser)

    /** parse between values. Since values are `Parser` the result is
      */
    def between(b: Parser[Any], c: Parser[Any]): Parser[A] =
      (b.void ~ (parser ~ c.void)).map { case (_, (a, _)) => a }

    /** parse surrounded by that. Since that is a Parser the result is
      */
    def surroundedBy(that: Parser[Any]): Parser[A] =
      between(that, that)
  }

  /** If we can parse this then that, do so, if we fail that without consuming, rewind before this
    * without consuming. If either consume 1 or more, do not rewind
    */
  sealed class Soft0[+A](parser: Parser0[A]) {
    def ~[B](that: Parser0[B]): Parser0[(A, B)] =
      softProduct0(parser, that)

    def *>[B](that: Parser0[B]): Parser0[B] =
      softProduct0(void0(parser), that).map(_._2)

    def <*[B](that: Parser0[B]): Parser0[A] =
      softProduct0(parser, void0(that)).map(_._1)

    /** If we can parse this then that, do so, if we fail that without consuming, rewind before this
      * without consuming. If either consume 1 or more, do not rewind
      */
    def with1: Soft01[A] =
      new Soft01(parser)

    /** Like regular between, but uses soft products This is particularly useful with whitespace and
      * separators, e.g. a separator might be `char(',').soft.surroundedBy(whitespace0)`
      */
    def between(b: Parser0[Any], c: Parser0[Any]): Parser0[A] =
      (b.void.soft ~ (parser.soft ~ c.void)).map { case (_, (a, _)) => a }

    /** Use this parser to parse surrounded by values using soft products
      *
      * This is the same as `between(b, b)`
      */
    def surroundedBy(b: Parser0[Any]): Parser0[A] =
      between(b, b)
  }

  /** If we can parse this then that, do so, if we fail that without consuming, rewind before this
    * without consuming. If either consume 1 or more, do not rewind
    */
  final class Soft[+A](parser: Parser[A]) extends Soft0(parser) {
    override def ~[B](that: Parser0[B]): Parser[(A, B)] =
      softProduct10(parser, that)

    override def *>[B](that: Parser0[B]): Parser[B] =
      softProduct10(void(parser), that).map(_._2)

    override def <*[B](that: Parser0[B]): Parser[A] =
      softProduct10(parser, void0(that)).map(_._1)

    override def between(b: Parser0[Any], c: Parser0[Any]): Parser[A] =
      (b.void.with1.soft ~ (parser.soft ~ c.void)).map { case (_, (a, _)) => a }

    override def surroundedBy(b: Parser0[Any]): Parser[A] =
      between(b, b)
  }

  /** If we can parse this then that, do so, if we fail that without consuming, rewind before this
    * without consuming. If either consume 1 or more, do not rewind
    */
  final class Soft01[+A](val parser: Parser0[A]) extends AnyVal {
    def ~[B](that: Parser[B]): Parser[(A, B)] =
      softProduct01(parser, that)

    def *>[B](that: Parser[B]): Parser[B] =
      softProduct01(void0(parser), that).map(_._2)

    def <*[B](that: Parser[B]): Parser[A] =
      softProduct01(parser, void(that)).map(_._1)

    def between(b: Parser[Any], c: Parser[Any]): Parser[A] =
      (b.void.soft ~ (parser.soft ~ c.void)).map { case (_, (a, _)) => a }

    def surroundedBy(b: Parser[Any]): Parser[A] =
      between(b, b)
  }

  /** Don't advance in the parsed string, just return a This is used by the Applicative typeclass.
    */
  def pure[A](a: A): Parser0[A] =
    Impl.Pure(a)

  /** Parse a given string, in a case-insensitive manner, or fail. This backtracks on failure this
    * is an error if the string is empty
    */
  def ignoreCase(str: String): Parser[Unit] =
    if (str.length == 1) {
      ignoreCaseChar(str.charAt(0))
    } else Impl.IgnoreCase(str.toLowerCase)

  /** Ignore the case of a single character If you want to know if it is upper or lower, use .string
    * to capture the string and then map to process the result.
    */
  def ignoreCaseChar(c: Char): Parser[Unit] =
    charIn(c.toLower, c.toUpper).void

  /** Parse a given string or fail. This backtracks on failure this is an error if the string is
    * empty
    */
  def string(str: String): Parser[Unit] =
    if (str.length == 1) char(str.charAt(0))
    else Impl.Str(str)

  /** Parse a potentially empty string or fail. This backtracks on failure
    */
  def string0(str: String): Parser0[Unit] =
    if (str.length == 0) unit
    else string(str)

  /** Parse a potentially empty string, in a case-insensitive manner, or fail. This backtracks on
    * failure
    */
  def ignoreCase0(str: String): Parser0[Unit] =
    if (str.length == 0) unit
    else ignoreCase(str)

  /** go through the list of parsers trying each as long as they are epsilon failures (don't
    * advance) see @backtrack if you want to do backtracking.
    *
    * This is the same as parsers.foldLeft(fail)(_.orElse(_))
    *
    * recommended style: oneOf(p1 :: p2 :: p3 :: Nil) rather than oneOf(List(p1, p2, p3))
    *
    * Note: order matters here, since we don't backtrack by default.
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

    val flattened = flatten(parsers, new ListBuffer)
    // we unmap if we can to make merging work better
    val isStr = flattened.forall(Impl.matchesString)
    val maybeUnmap = if (isStr) flattened.map(Impl.unmap) else flattened

    val cs = Impl.mergeCharIn[Any, Parser[Any]](maybeUnmap)
    val res = Impl.mergeStrIn[Any, Parser[Any]](cs) match {
      case Nil => fail
      case p :: Nil => p
      case two => Impl.OneOf(two)
    }

    (if (isStr) string(res) else res).asInstanceOf[Parser[A]]
  }

  /** go through the list of parsers trying each as long as they are epsilon failures (don't
    * advance) see @backtrack if you want to do backtracking.
    *
    * This is the same as parsers.foldLeft(fail)(_.orElse(_))
    *
    * recommended style: oneOf(p1 :: p2 :: p3 :: Nil) rather than oneOf(List(p1, p2, p3))
    *
    * Note: order matters here, since we don't backtrack by default.
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

    val flat0 = flatten(ps, new ListBuffer)
    // we unmap if we can to make merging work better
    val isStr = flat0.forall(Impl.matchesString)
    val flat = if (isStr) flat0.map(Impl.unmap0) else flat0

    val cs = Impl.mergeCharIn[Any, Parser0[Any]](flat)
    val res = Impl.mergeStrIn[Any, Parser0[Any]](cs) match {
      case Nil => fail
      case p :: Nil => p
      case two => Impl.OneOf0(two)
    }

    (if (isStr) string0(res) else res).asInstanceOf[Parser0[A]]
  }

  /** Parse the longest matching string between alternatives. The order of the strings does not
    * matter.
    *
    * If no string matches, this parser results in an epsilon failure.
    *
    * It is an error to pass the empty string here, if you need that see stringIn0
    */
  def stringIn(strings: Iterable[String]): Parser[String] =
    strings.toList.distinct match {
      case Nil => fail
      case s :: Nil => string(s).string
      case two =>
        Impl
          .StringIn(
            SortedSet(two: _*)
          ) // sadly scala 2.12 doesn't have the `SortedSet.from` constructor function
          .string
    }

  /** Version of stringIn that allows the empty string
    */
  def stringIn0(strings: Iterable[String]): Parser0[String] =
    if (strings.exists(_.isEmpty)) stringIn(strings.filter(_.nonEmpty)).orElse(emptyStringParser0)
    else stringIn(strings)

  /** If the first parser fails to parse its input with an epsilon error, try the second parser
    * instead.
    *
    * If the first parser fails with an arresting error, the second parser won't be tried.
    *
    * Backtracking may be used on the first parser to allow the second one to pick up after any
    * error, resetting any state that was modified by the first parser.
    */
  def eitherOr0[A, B](first: Parser0[B], second: Parser0[A]): Parser0[Either[A, B]] =
    oneOf0(first.map(Right(_)) :: second.map(Left(_)) :: Nil)

  /** If the first parser fails to parse its input with an epsilon error, try the second parser
    * instead.
    *
    * If the first parser fails with an arresting error, the second parser won't be tried.
    *
    * Backtracking may be used on the first parser to allow the second one to pick up after any
    * error, resetting any state that was modified by the first parser.
    */
  def eitherOr[A, B](first: Parser[B], second: Parser[A]): Parser[Either[A, B]] =
    oneOf(first.map(Right(_)) :: second.map(Left(_)) :: Nil)

  private[this] val emptyStringParser0: Parser0[String] =
    pure("")

  private[parse] val optTail: List[Parser0[Option[Nothing]]] = Parser.pure(None) :: Nil

  /** if len < 1, the same as pure("") else length(len)
    */
  def length0(len: Int): Parser0[String] =
    if (len > 0) length(len) else emptyStringParser0

  /** Parse the next len characters where len > 0 if (len < 1) throw IllegalArgumentException
    */
  def length(len: Int): Parser[String] =
    Impl.Length(len)

  /** Repeat the parser 0 or more times
    *
    * @note
    *   this can wind up parsing nothing
    */
  def repAs0[A, B](p1: Parser[A])(implicit acc: Accumulator0[A, B]): Parser0[B] =
    Impl.Rep0(p1, Int.MaxValue, acc)

  /** Repeat the parser 0 or more times, but no more than `max`
    *
    * It may seem weird to accept 0 here, but without, composing this method becomes more complex.
    * Since and empty parse is possible for this method, we do allow max = 0
    *
    * @throws java.lang.IllegalArgumentException
    *   if max < 0
    *
    * @note
    *   this can wind up parsing nothing
    */
  def repAs0[A, B](p1: Parser[A], max: Int)(implicit acc: Accumulator0[A, B]): Parser0[B] = {
    require(max >= 0, s"max should be >= 0, was $max")
    if (max == 0) {
      // exactly 0 times
      pure(acc.newAppender().finish())
    } else {
      // 0 or more times
      Impl.Rep0(p1, max - 1, acc)
    }
  }

  /** Repeat the parser `min` or more times
    *
    * The parser fails if it can't match at least `min` times
    *
    * @throws java.lang.IllegalArgumentException
    *   if min < 1
    */
  def repAs[A, B](p1: Parser[A], min: Int)(implicit acc: Accumulator[A, B]): Parser[B] = {
    require(min >= 1, s"min should be >= 1, was $min")
    Impl.Rep(p1, min, Int.MaxValue, acc)
  }

  /** Repeat the parser `min` or more times, but no more than `max`
    *
    * The parser fails if it can't match at least `min` times After repeating the parser `max`
    * times, the parser completes succesfully
    *
    * @throws java.lang.IllegalArgumentException
    *   if min < 1 or max < min
    */
  def repAs[A, B](p1: Parser[A], min: Int, max: Int)(implicit acc: Accumulator[A, B]): Parser[B] = {
    require(min >= 1, s"min should be >= 1, was $min")

    if (min == max) repExactlyAs(p1, min)
    else {
      require(max > min, s"max should be >= min, but $max < $min")
      Impl.Rep(p1, min, max - 1, acc)
    }
  }

  /** Repeat the parser exactly `times` times
    *
    * @throws java.lang.IllegalArgumentException
    *   if times < 1
    */
  def repExactlyAs[A, B](p: Parser[A], times: Int)(implicit acc: Accumulator[A, B]): Parser[B] =
    if (times == 1) {
      // a map can be removed by a subsequent .void or .string
      p.map { a => acc.newAppender(a).finish() }
    } else {
      require(times > 1, s"times should be >= 1, was $times")
      Impl.Rep(p, times, times - 1, acc)
    }

  /** Repeat 1 or more times with a separator
    */
  def repSep[A](p1: Parser[A], sep: Parser0[Any]): Parser[NonEmptyList[A]] =
    repSep(p1, min = 1, sep)

  /** Repeat `min` or more times with a separator, at least once.
    *
    * @throws java.lang.IllegalArgumentException
    *   if `min <= 0`
    */
  def repSep[A](p1: Parser[A], min: Int, sep: Parser0[Any]): Parser[NonEmptyList[A]] = {
    // we validate here so the message matches what the user passes
    // instead of transforming to min - 1 below
    if (min <= 0) throw new IllegalArgumentException(s"require min > 0, found: $min")

    val rest = (sep.void.with1.soft *> p1).rep0(min - 1)
    (p1 ~ rest).map { case (h, t) => NonEmptyList(h, t) }
  }

  /** Repeat `min` or more, up to `max` times with a separator, at least once.
    *
    * @throws java.lang.IllegalArgumentException
    *   if `min <= 0` or `max < min`
    */
  def repSep[A](p1: Parser[A], min: Int, max: Int, sep: Parser0[Any]): Parser[NonEmptyList[A]] = {
    // we validate here so the message matches what the user passes
    // instead of transforming to min - 1 below
    if (min <= 0) throw new IllegalArgumentException(s"require min > 0, found: $min")
    if (max < min) throw new IllegalArgumentException(s"require max >= min, found: $max < $min")

    if ((min == 1) && (max == 1)) {
      p1.map(NonEmptyList(_, Nil))
    } else {
      val rest = (sep.void.with1.soft *> p1).rep0(min = min - 1, max = max - 1)
      (p1 ~ rest).map { case (h, t) => NonEmptyList(h, t) }
    }
  }

  /** Repeat 0 or more times with a separator
    */
  def repSep0[A](p1: Parser[A], sep: Parser0[Any]): Parser0[List[A]] =
    repSep0(p1, 0, sep)

  /** Repeat `min` or more times with a separator.
    *
    * @throws java.lang.IllegalArgumentException
    *   if `min < 0`
    */
  def repSep0[A](p1: Parser[A], min: Int, sep: Parser0[Any]): Parser0[List[A]] =
    if (min == 0) repSep(p1, sep).?.map {
      case None => Nil
      case Some(nel) => nel.toList
    }
    else repSep(p1, min, sep).map(_.toList)

  /** Repeat `min` or more, up to `max` times with a separator.
    *
    * @throws java.lang.IllegalArgumentException
    *   if `min < 0` or `max < min`
    */
  def repSep0[A](p1: Parser[A], min: Int, max: Int, sep: Parser0[Any]): Parser0[List[A]] =
    if (min == 0) {
      if (max == 0) pure(Nil)
      else
        repSep(p1, 1, max, sep).?.map {
          case None => Nil
          case Some(nel) => nel.toList
        }
    } else repSep(p1, min, max, sep).map(_.toList)

  /** parse first then second
    */
  def product0[A, B](first: Parser0[A], second: Parser0[B]): Parser0[(A, B)] =
    first match {
      case f1: Parser[A] => product10(f1, second)
      case _ =>
        second match {
          case s1: Parser[B] =>
            product01(first, s1)
          case _ => Impl.Prod0(first, second)
        }
    }

  /** product with the first argument being a Parser
    */
  def product10[A, B](first: Parser[A], second: Parser0[B]): Parser[(A, B)] =
    first match {
      case f @ Impl.Fail() => f.widen
      case f @ Impl.FailWith(_) => f.widen
      case _ => Impl.Prod(first, second)
    }

  /** product with the second argument being a Parser
    */
  def product01[A, B](first: Parser0[A], second: Parser[B]): Parser[(A, B)] =
    first match {
      case p1: Parser[A] => product10(p1, second)
      case _ => Impl.Prod(first, second)
    }

  /** softProduct, a variant of product A soft product backtracks if the first succeeds and the
    * second is an epsilon-failure. By contrast product will be a failure in that case
    *
    * see @Parser.soft
    */
  def softProduct0[A, B](first: Parser0[A], second: Parser0[B]): Parser0[(A, B)] =
    first match {
      case f1: Parser[A] => softProduct10(f1, second)
      case _ =>
        second match {
          case s1: Parser[B] =>
            softProduct01(first, s1)
          case _ => Impl.SoftProd0(first, second)
        }
    }

  /** softProduct with the first argument being a Parser A soft product backtracks if the first
    * succeeds and the second is an epsilon-failure. By contrast product will be a failure in that
    * case
    *
    * see @Parser.soft
    */
  def softProduct10[A, B](first: Parser[A], second: Parser0[B]): Parser[(A, B)] =
    first match {
      case f @ Impl.Fail() => f.widen
      case f @ Impl.FailWith(_) => f.widen
      case _ => Impl.SoftProd(first, second)
    }

  /** softProduct with the second argument being a Parser A soft product backtracks if the first
    * succeeds and the second is an epsilon-failure. By contrast product will be a failure in that
    * case
    *
    * see @Parser.soft
    */
  def softProduct01[A, B](first: Parser0[A], second: Parser[B]): Parser[(A, B)] =
    first match {
      case f @ Impl.Fail() => f.widen
      case f @ Impl.FailWith(_) => f.widen
      case _ => Impl.SoftProd(first, second)
    }

  /** transform a Parser0 result
    */
  def map0[A, B](p: Parser0[A])(fn: A => B): Parser0[B] =
    p match {
      case p1: Parser[A] => map(p1)(fn)
      case Impl.Pure(a) => Impl.Pure(fn(a))
      case Impl.Map0(p0, f0) =>
        val f1 = f0 match {
          case Impl.ConstFn(a) => Impl.ConstFn(fn(a))
          case _ => AndThen(f0).andThen(fn)
        }
        Impl.Map0(p0, f1)
      case _ => Impl.Map0(p, fn)
    }

  /** transform a Parser result
    */
  def map[A, B](p: Parser[A])(fn: A => B): Parser[B] =
    p match {
      case Impl.Map(p0, f0) =>
        val f1 = f0 match {
          case Impl.ConstFn(a) => Impl.ConstFn(fn(a))
          case _ => AndThen(f0).andThen(fn)
        }
        Impl.Map(p0, f1)
      case f @ Impl.Fail() => f.widen
      case f @ Impl.FailWith(_) => f.widen
      case _ => Impl.Map(p, fn)
    }

  /** Parse p and if we get the Left side, parse fn This function name comes from seletive functors.
    * This should be more efficient than flatMap since the fn Parser0 is evaluated once, not on
    * every item parsed
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

  /** Standard monadic flatMap Avoid this function if possible. If you can instead use product, ~,
    * *>, or <* use that. flatMap always has to allocate a parser, and the parser is less amenable
    * to optimization
    */
  def flatMap0[A, B](pa: Parser0[A])(fn: A => Parser0[B]): Parser0[B] =
    pa match {
      case p: Parser[A] => flatMap10(p)(fn)
      case _ => Impl.FlatMap0(pa, fn)
    }

  /** Standard monadic flatMap where you start with a Parser Avoid this function if possible. If you
    * can instead use product, ~, *>, or <* use that. flatMap always has to allocate a parser, and
    * the parser is less amenable to optimization
    */
  def flatMap10[A, B](pa: Parser[A])(fn: A => Parser0[B]): Parser[B] =
    pa match {
      case f @ Impl.Fail() => f.widen
      case f @ Impl.FailWith(_) => f.widen
      case _ => Impl.FlatMap(pa, fn)
    }

  /** Standard monadic flatMap where you end with a Parser Avoid this function if possible. If you
    * can instead use product, ~, *>, or <* use that. flatMap always has to allocate a parser, and
    * the parser is less amenable to optimization
    */
  def flatMap01[A, B](pa: Parser0[A])(fn: A => Parser[B]): Parser[B] =
    pa match {
      case p1: Parser[A] => flatMap10(p1)(fn)
      case _ => Impl.FlatMap(pa, fn)
    }

  /** tail recursive monadic flatMaps This is a rarely used function, but needed to implement
    * cats.FlatMap Avoid this function if possible. If you can instead use product, ~, *>, or <* use
    * that. flatMap always has to allocate a parser, and the parser is less amenable to optimization
    */
  def tailRecM0[A, B](init: A)(fn: A => Parser0[Either[A, B]]): Parser0[B] =
    Impl.TailRecM0(init, fn)

  /** tail recursive monadic flatMaps on Parser This is a rarely used function, but needed to
    * implement cats.FlatMap Avoid this function if possible. If you can instead use product, ~, *>,
    * or <* use that. flatMap always has to allocate a parser, and the parser is less amenable to
    * optimization
    */
  def tailRecM[A, B](init: A)(fn: A => Parser[Either[A, B]]): Parser[B] =
    Impl.TailRecM(init, fn)

  /** Lazily create a Parser This is useful to create some recursive parsers see Defer0[Parser].fix
    */
  def defer[A](pa: => Parser[A]): Parser[A] =
    Impl.Defer(() => pa)

  /** Lazily create a Parser0 This is useful to create some recursive parsers see Defer0[Parser].fix
    */
  def defer0[A](pa: => Parser0[A]): Parser0[A] =
    Impl.Defer0(() => pa)

  /** Build a recursive parser by assuming you have it Useful for parsing recurive structures, like
    * for instance JSON.
    */
  def recursive[A](fn: Parser[A] => Parser[A]): Parser[A] = {
    lazy val result: Parser[A] = fn(defer(result))
    result
  }

  /** A parser that always fails with an epsilon failure
    */
  val Fail: Parser[Nothing] = Impl.Fail()

  /** A parser that always fails with an epsilon failure
    */
  def fail[A]: Parser[A] = Fail

  /** A parser that always fails with an epsilon failure and a given message this is generally used
    * with flatMap to validate a result beyond the literal parsing.
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
      rangesFor(ary) match {
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
    fn match {
      case s: Set[Char] =>
        // Set extends function, but it is also iterable
        charIn(s)
      case _ =>
        charIn(Impl.allChars.filter(fn))
    }

  /** Parse a string while the given function is true
    */
  def charsWhile0(fn: Char => Boolean): Parser0[String] =
    charWhere(fn).rep0.string

  /** Parse a string while the given function is true parses at least one character
    */
  def charsWhile(fn: Char => Boolean): Parser[String] =
    charWhere(fn).rep.string

  /** parse zero or more characters as long as they don't match p this is useful for parsing comment
    * strings, for instance.
    */
  def until0(p: Parser0[Any]): Parser0[String] =
    (not(p).with1 ~ anyChar).rep0.string

  /** parse one or more characters as long as they don't match p
    */
  def until(p: Parser0[Any]): Parser[String] =
    (not(p).with1 ~ anyChar).rep.string

  /** parse zero or more times until Parser `end` succeeds.
    */
  def repUntil0[A](p: Parser[A], end: Parser0[Any]): Parser0[List[A]] =
    (not(end).with1 *> p).rep0

  /** parse one or more times until Parser `end` succeeds.
    */
  def repUntil[A](p: Parser[A], end: Parser0[Any]): Parser[NonEmptyList[A]] =
    (not(end).with1 *> p).rep

  /** parse zero or more times until Parser `end` succeeds.
    */
  def repUntilAs0[A, B](p: Parser[A], end: Parser0[Any])(implicit
      acc: Accumulator0[A, B]
  ): Parser0[B] =
    (not(end).with1 *> p).repAs0

  /** parse one or more times until Parser `end` succeeds.
    */
  def repUntilAs[A, B](p: Parser[A], end: Parser0[Any])(implicit
      acc: Accumulator[A, B]
  ): Parser[B] =
    (not(end).with1 *> p).repAs

  /** discard the value in a Parser. This is an optimization because we remove trailing map
    * operations and don't allocate internal data structures This function is called internal to
    * Functor.as and Apply.*> and Apply.<* so those are good uses.
    */
  def void0(pa: Parser0[Any]): Parser0[Unit] =
    pa match {
      case v @ Impl.Void0(_) => v
      case p1: Parser[_] => void(p1)
      case s if Impl.alwaysSucceeds(s) => unit
      case _ =>
        Impl.unmap0(pa) match {
          case Impl.StartParser => Impl.StartParser
          case Impl.EndParser => Impl.EndParser
          case n @ Impl.Not(_) => n
          case p @ Impl.Peek(_) => p
          case other => Impl.Void0(other)
        }
    }

  /** discard the value in a Parser. This is an optimization because we remove trailing map
    * operations and don't allocate internal data structures This function is called internal to
    * Functor.as and Apply.*> and Apply.<* so those are good uses.
    */
  def void(pa: Parser[Any]): Parser[Unit] =
    pa match {
      case v @ Impl.Void(_) => v
      case _ =>
        Impl.unmap(pa) match {
          case f @ Impl.Fail() => f.widen
          case f @ Impl.FailWith(_) => f.widen
          case p: Impl.Str => p
          case p: Impl.StringIn => p
          case p: Impl.IgnoreCase => p
          case notVoid => Impl.Void(notVoid)
        }
    }

  /** Discard the result A and instead capture the matching string this is optimized to avoid
    * internal allocations
    */
  def string0(pa: Parser0[Any]): Parser0[String] =
    pa match {
      case s1: Parser[_] => string(s1)
      case str if Impl.matchesString(str) => str.asInstanceOf[Parser0[String]]
      case _ =>
        Impl.unmap0(pa) match {
          case Impl.Pure(_) | Impl.Index | Impl.GetCaret => emptyStringParser0
          case notEmpty => Impl.StringP0(notEmpty)
        }
    }

  /** Discard the result A and instead capture the matching string this is optimized to avoid
    * internal allocations
    */
  def string(pa: Parser[Any]): Parser[String] =
    pa match {
      case str if Impl.matchesString(str) => str.asInstanceOf[Parser[String]]
      case _ =>
        Impl.unmap(pa) match {
          case len @ Impl.Length(_) => len
          case strP @ Impl.Str(expect) => strP.as(expect)
          case ci @ Impl.CharIn(min, bs, _) if BitSetUtil.isSingleton(bs) =>
            // we can allocate the returned string once here
            val minStr = min.toChar.toString
            Impl.Map(ci, Impl.ConstFn(minStr))
          case f @ Impl.Fail() => f.widen
          case f @ Impl.FailWith(_) => f.widen
          case notStr => Impl.StringP(notStr)
        }
    }

  /** returns a parser that succeeds if the current parser fails. Note, this parser backtracks
    * (never returns an arresting failure)
    */
  def not(pa: Parser0[Any]): Parser0[Unit] =
    void0(pa) match {
      case Impl.Fail() | Impl.FailWith(_) => unit
      case notFail => Impl.Not(notFail)
    }

  /** a parser that consumes nothing when it succeeds, basically rewind on success
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

  /** return the current position in the string we are parsing. This lets you record position
    * information in your ASTs you are parsing
    */
  def index: Parser0[Int] = Impl.Index

  /** return the current Caret (offset, line, column) this is a bit more expensive that just the
    * index
    */
  def caret: Parser0[Caret] = Impl.GetCaret

  /** succeeds when we are at the start
    */
  def start: Parser0[Unit] = Impl.StartParser

  /** succeeds when we are at the end
    */
  def end: Parser0[Unit] = Impl.EndParser

  /** If we fail, rewind the offset back so that we can try other branches. This tends to harm
    * debuggability and ideally should be minimized
    */
  def backtrack0[A](pa: Parser0[A]): Parser0[A] =
    pa match {
      case p1: Parser[A] => backtrack(p1)
      case pa if Impl.doesBacktrack(pa) => pa
      case nbt => Impl.Backtrack0(nbt)
    }

  /** If we fail, rewind the offset back so that we can try other branches. This tends to harm
    * debuggability and ideally should be minimized
    */
  def backtrack[A](pa: Parser[A]): Parser[A] =
    pa match {
      case pa if Impl.doesBacktrack(pa) => pa
      case nbt => Impl.Backtrack(nbt)
    }

  /** Replaces parsed values with the given value.
    */
  def as0[B](pa: Parser0[Any], b: B): Parser0[B] =
    pa.void match {
      case p1: Parser[_] => as(p1, b)
      case _ =>
        Impl.unmap0(pa) match {
          case Impl.Pure(_) | Impl.Index | Impl.GetCaret => pure(b)
          case notPure =>
            Impl.Void0(notPure).map(Impl.ConstFn(b))
        }
    }

  /** Replaces parsed values with the given value.
    */
  def as[B](pa: Parser[Any], b: B): Parser[B] = {
    pa.void match {
      case Impl.Void(ci @ Impl.CharIn(min, bs, _)) =>
        // CharIn is common and cheap, no need to wrap
        // with Void since CharIn always returns the char
        // even when voided
        b match {
          case bc: Char if BitSetUtil.isSingleton(bs) && (min.toChar == bc) =>
            ci.asInstanceOf[Parser[B]]
          case _ =>
            Impl.Map(ci, Impl.ConstFn(b))
        }
      case notSingleChar => notSingleChar.map(Impl.ConstFn(b))
    }
  }

  /** Add a context string to Errors to aid debugging
    */
  def withContext0[A](p0: Parser0[A], ctx: String): Parser0[A] =
    Impl.WithContextP0(ctx, p0)

  /** Add a context string to Errors to aid debugging
    */
  def withContext[A](p: Parser[A], ctx: String): Parser[A] =
    Impl.WithContextP(ctx, p)

  implicit val catsInstancesParser
      : FlatMap[Parser] with Defer[Parser] with MonoidK[Parser] with FunctorFilter[Parser] =
    new FlatMap[Parser] with Defer[Parser] with MonoidK[Parser] with FunctorFilter[Parser] {
      override def empty[A] = Fail

      override def defer[A](pa: => Parser[A]): Parser[A] =
        Parser.this.defer(pa)

      override def functor = this

      override def map[A, B](fa: Parser[A])(fn: A => B): Parser[B] =
        Parser.this.map(fa)(fn)

      override def mapFilter[A, B](fa: Parser[A])(f: A => Option[B]): Parser[B] =
        fa.mapFilter(f)

      override def filter[A](fa: Parser[A])(fn: A => Boolean): Parser[A] =
        fa.filter(fn)

      override def flatMap[A, B](fa: Parser[A])(fn: A => Parser[B]): Parser[B] =
        Parser.this.flatMap10(fa)(fn)

      override def product[A, B](pa: Parser[A], pb: Parser[B]): Parser[(A, B)] =
        Parser.this.product10(pa, pb)

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

      override def tailRecM[A, B](init: A)(fn: A => Parser[Either[A, B]]): Parser[B] =
        Parser.this.tailRecM(init)(fn)

      override def combineK[A](pa: Parser[A], pb: Parser[A]): Parser[A] =
        Parser.oneOf(pa :: pb :: Nil)

      override def void[A](pa: Parser[A]): Parser[Unit] =
        pa.void

      override def as[A, B](pa: Parser[A], b: B): Parser[B] =
        Parser.as(pa, b)

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

  /*
   * This is protected rather than private to avoid a warning on 2.12
   */
  protected[parse] final class State(val str: String) {
    var offset: Int = 0
    var error: Eval[Chain[Expectation]] = null
    var capture: Boolean = true

    // This is lazy because we don't want to trigger it
    // unless someone uses GetCaret
    lazy val locationMap: LocationMap = LocationMap(str)
  }

  // invariant: input must be sorted
  private[parse] def rangesFor(charArray: Array[Char]): NonEmptyList[(Char, Char)] = {
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

  private object Impl {

    val nilError: Eval[Chain[Expectation]] = Eval.now(Chain.nil)

    val allChars = Char.MinValue to Char.MaxValue

    case class ConstFn[A](result: A) extends Function[Any, A] {
      def apply(any: Any) = result

      override def andThen[B](that: Function[A, B]): ConstFn[B] =
        ConstFn(that(result))
    }

    // this is used to make def unmap0 a pure function wrt `def equals`
    case class UnmapDefer0(fn: () => Parser0[Any]) extends Function0[Parser0[Any]] {
      def apply(): Parser0[Any] = unmap0(compute0(fn))
    }

    // this is used to make def unmap a pure function wrt `def equals`
    case class UnmapDefer(fn: () => Parser[Any]) extends Function0[Parser[Any]] {
      def apply(): Parser[Any] = unmap(compute(fn))
    }

    final def doesBacktrackCheat(p: Parser0[Any]): Boolean =
      doesBacktrack(p)

    @annotation.tailrec
    final def doesBacktrack(p: Parser0[Any]): Boolean =
      p match {
        case Backtrack0(_) | Backtrack(_) | AnyChar | CharIn(_, _, _) | Str(_) | IgnoreCase(_) |
            Length(_) | StartParser | EndParser | Index | GetCaret | Pure(_) | Fail() | FailWith(
              _
            ) | Not(_) | StringIn(_) =>
          true
        case Map0(p, _) => doesBacktrack(p)
        case Map(p, _) => doesBacktrack(p)
        case SoftProd0(a, b) => doesBacktrackCheat(a) && doesBacktrack(b)
        case SoftProd(a, b) => doesBacktrackCheat(a) && doesBacktrack(b)
        case WithContextP(_, p) => doesBacktrack(p)
        case WithContextP0(_, p) => doesBacktrack(p)
        case _ => false
      }

    // does this parser return the string it matches
    def matchesString(p: Parser0[Any]): Boolean =
      p match {
        case StringP0(_) | StringP(_) | Pure("") | Length(_) | Fail() | FailWith(_) => true
        case Map(Str(e1), ConstFn(e2)) => e1 == e2
        case Map(CharIn(min, bs, _), ConstFn(e)) if BitSetUtil.isSingleton(bs) =>
          e == min.toChar.toString
        case OneOf(ss) => ss.forall(matchesString)
        case OneOf0(ss) => ss.forall(matchesString)
        case WithContextP(_, p) => matchesString(p)
        case WithContextP0(_, p) => matchesString(p)
        case _ => false
      }

    // does this parser always succeed?
    // note: a parser1 does not always succeed
    // and by construction, a oneOf0 never always succeeds
    final def alwaysSucceeds(p: Parser0[Any]): Boolean =
      p match {
        case Index | GetCaret | Pure(_) => true
        case Map0(p, _) => alwaysSucceeds(p)
        case SoftProd0(a, b) => alwaysSucceeds(a) && alwaysSucceeds(b)
        case Prod0(a, b) => alwaysSucceeds(a) && alwaysSucceeds(b)
        case WithContextP0(_, p) => alwaysSucceeds(p)
        // by construction we never build a Not(Fail()) since
        // it would just be the same as unit
        // case Not(Fail() | FailWith(_)) => true
        case _ => false
      }

    /** This removes any trailing map functions which can cause wasted allocations if we are later
      * going to void or return strings. This stops at StringP or VoidP since those are markers that
      * anything below has already been transformed
      */
    def unmap0(pa: Parser0[Any]): Parser0[Any] =
      pa match {
        case p1: Parser[Any] => unmap(p1)
        case GetCaret | Index | Pure(_) => Parser.unit
        case s if alwaysSucceeds(s) => Parser.unit
        case Map0(p, _) =>
          // we discard any allocations done by fn
          unmap0(p)
        case Select0(p, fn) =>
          Select0(p, unmap0(fn))
        case StringP0(s) =>
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
          Parser.backtrack0(unmap0(p))
        case OneOf0(ps) => Parser.oneOf0(ps.map(unmap0))
        case Prod0(p1, p2) =>
          unmap0(p1) match {
            case Prod0(p11, p12) =>
              // right associate so
              // we can check matches a bit faster
              // note: p12 is already unmapped, so
              // we wrap with Void to prevent n^2 cost
              Prod0(p11, unmap0(Prod0(Void0(p12), p2)))
            case u1 if u1 eq Parser.unit =>
              unmap0(p2)
            case u1 =>
              val u2 = unmap0(p2)
              if (u2 eq Parser.unit) u1
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
            case u1 if u1 eq Parser.unit =>
              unmap0(p2)
            case u1 =>
              val u2 = unmap0(p2)
              if (u2 eq Parser.unit) u1
              else SoftProd0(u1, u2)
          }
        case Defer0(fn) =>
          fn match {
            case UnmapDefer0(_) => pa // already unmapped
            case _ => Defer0(UnmapDefer0(fn))
          }
        case Rep0(p, max, _) => Rep0(unmap(p), max, Accumulator0.unitAccumulator0)
        case WithContextP0(ctx, p0) => WithContextP0(ctx, unmap0(p0))
        case StartParser | EndParser | TailRecM0(_, _) | FlatMap0(_, _) =>
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

    /** This removes any trailing map functions which can cause wasted allocations if we are later
      * going to void or return strings. This stops at StringP or VoidP since those are markers that
      * anything below has already been transformed
      */
    def unmap(pa: Parser[Any]): Parser[Any] =
      pa match {
        case Map(p, _) =>
          // we discard any allocations done by fn
          unmap(p)
        case Select(p, fn) =>
          Select(p, unmap0(fn))
        case StringP(s) =>
          // StringP is added privately, and only after unmap
          s
        case Void(v) =>
          // Void is added privately, and only after unmap
          v
        case Backtrack(p) =>
          // unmap may simplify enough
          // to remove the backtrack wrapper
          Parser.backtrack(unmap(p))
        case OneOf(ps) => Parser.oneOf(ps.map(unmap))
        case Prod(p1, p2) =>
          unmap0(p1) match {
            case Prod0(p11, p12) =>
              // right associate so
              // we can check matches a bit faster
              // note: p12 is already unmapped, so
              // we wrap with Void to prevent n^2 cost
              Prod(p11, unmap0(Parser.product0(p12.void, p2)))
            case Prod(p11, p12) =>
              // right associate so
              // we can check matches a bit faster
              // we wrap with Void to prevent n^2 cost
              Prod(p11, unmap0(Parser.product0(p12.void, p2)))
            case u1 if u1 eq Parser.unit =>
              // if unmap0(u1) is unit, p2 must be a Parser
              unmap(expect1(p2))
            case u1 =>
              val u2 = unmap0(p2)
              if (u2 eq Parser.unit) expect1(u1)
              else Prod(u1, u2)
          }
        case SoftProd(p1, p2) =>
          unmap0(p1) match {
            case SoftProd0(p11, p12) =>
              // right associate so
              // we can check matches a bit faster
              // we wrap with Void to prevent n^2 cost
              SoftProd(p11, unmap0(Parser.softProduct0(p12.void, p2)))
            case SoftProd(p11, p12) =>
              // right associate so
              // we can check matches a bit faster
              // we wrap with Void to prevent n^2 cost
              SoftProd(p11, unmap0(Parser.softProduct0(p12.void, p2)))
            case u1 if u1 eq Parser.unit =>
              // if unmap0(u1) is unit, p2 must be a Parser
              unmap(expect1(p2))
            case u1 =>
              val u2 = unmap0(p2)
              if (u2 eq Parser.unit) expect1(u1)
              else SoftProd(u1, u2)
          }
        case Defer(fn) =>
          fn match {
            case UnmapDefer(_) => pa // already unmapped
            case _ => Defer(UnmapDefer(fn))
          }
        case Rep(p, min, max, _) => Rep(unmap(p), min, max, Accumulator0.unitAccumulator0)
        case WithContextP(ctx, p) =>
          WithContextP(ctx, unmap(p))
        case AnyChar | CharIn(_, _, _) | Str(_) | StringIn(_) | IgnoreCase(_) | Fail() | FailWith(
              _
            ) | Length(_) | TailRecM(_, _) | FlatMap(_, _) =>
          // we can't transform this significantly
          pa

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
          state.error =
            Eval.later(Chain.one(Expectation.Length(offset, len, state.str.length - offset)))
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

    case class StringP0[A](parser: Parser0[A]) extends Parser0[String] {
      override def parseMut(state: State): String =
        Impl.string0(parser, state)
    }

    case class StringP[A](parser: Parser[A]) extends Parser[String] {
      override def parseMut(state: State): String =
        Impl.string0(parser, state)
    }

    case object StartParser extends Parser0[Unit] {
      override def parseMut(state: State): Unit = {
        val offset = state.offset
        if (offset != 0) {
          state.error = Eval.later(Chain.one(Expectation.StartOfString(offset)))
        }
        ()
      }
    }

    case object EndParser extends Parser0[Unit] {
      override def parseMut(state: State): Unit = {
        val offset = state.offset
        if (offset != state.str.length) {
          state.error = Eval.later(Chain.one(Expectation.EndOfString(offset, state.str.length)))
        }
        ()
      }
    }

    case object Index extends Parser0[Int] {
      override def parseMut(state: State): Int = state.offset
    }

    case object GetCaret extends Parser0[Caret] {
      override def parseMut(state: State): Caret =
        // This unsafe call is safe because the offset can never go too far
        state.locationMap.toCaretUnsafe(state.offset)
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
          state.error = Eval.later(Chain.one(Expectation.OneOfStr(offset, message :: Nil)))
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
          state.error = Eval.later(Chain.one(Expectation.OneOfStr(offset, message :: Nil)))
          ()
        }
      }
    }

    case class Fail[A]() extends Parser[A] {
      override def parseMut(state: State): A = {
        val offset = state.offset
        state.error = Eval.later(Chain.one(Expectation.Fail(offset)))
        null.asInstanceOf[A]
      }

      def widen[B]: Parser[B] = this.asInstanceOf[Parser[B]]
    }

    case class FailWith[A](message: String) extends Parser[A] {
      override def parseMut(state: State): A = {
        val offset = state.offset
        state.error = Eval.later(Chain.one(Expectation.FailWith(offset, message)))
        null.asInstanceOf[A]
      }

      def widen[B]: Parser[B] = this.asInstanceOf[Parser[B]]
    }

    /*
     * We don't want to include empty Fail messages inside of a oneOf
     * since they are the zeros of the orElse operation
     */
    final def filterFails(offset: Int, fs: Chain[Expectation]): Chain[Expectation] = {
      val fs1 = fs.filter {
        case Expectation.Fail(o) if o == offset => false
        case _ => true
      }
      if (fs1.isEmpty) Chain.one(Expectation.Fail(offset))
      else fs1
    }

    final def oneOf[A](all: Array[Parser0[A]], state: State): A = {
      val offset = state.offset
      var errs: Eval[Chain[Expectation]] = nilError
      var idx = 0
      while (idx < all.length) {
        val thisParser = all(idx)
        val res = thisParser.parseMut(state)
        // we stop if there was no error
        // or if we consumed some input
        val err = state.error
        if ((err eq null) || (state.offset != offset)) {
          return res
        } else {
          // we failed to parse, but didn't consume input
          // is unchanged we continue
          // else we stop
          errs = for { e1 <- errs; e2 <- err } yield e1 ++ e2
          state.error = null
          idx = idx + 1
        }
      }
      // if we got here, all of them failed, but we
      // never advanced the offset
      state.error = errs.map(filterFails(offset, _))
      null.asInstanceOf[A]
    }

    final def stringIn[A](radix: RadixNode, all: SortedSet[String], state: State): Unit = {
      val str = state.str
      val strLength = str.length
      val startOffset = state.offset

      var offset = startOffset
      var tree = radix
      var cont = offset < strLength
      var lastMatch = -1

      while (cont) {
        val c = str.charAt(offset)
        val idx = Arrays.binarySearch(tree.fsts, c)
        if (idx >= 0) {
          val prefix = tree.prefixes(idx)
          // accept the prefix fo this character
          if (str.startsWith(prefix, offset + 1)) {
            val children = tree.children(idx)
            offset += (prefix.length + 1)
            tree = children
            cont = offset < strLength
            if (children.word) lastMatch = offset
          } else {
            cont = false
          }
        } else {
          cont = false
        }
      }
      if (lastMatch < 0) {
        state.error = Eval.later(Chain.one(Expectation.OneOfStr(startOffset, all.toList)))
        state.offset = startOffset
      } else {
        state.offset = lastMatch
      }
    }

    case class OneOf[A](all: List[Parser[A]]) extends Parser[A] {
      require(all.lengthCompare(2) >= 0, s"expected more than two items, found: ${all.size}")
      private[this] val ary: Array[Parser0[A]] = all.toArray

      override def parseMut(state: State): A = oneOf(ary, state)
    }

    case class OneOf0[A](all: List[Parser0[A]]) extends Parser0[A] {
      require(all.lengthCompare(2) >= 0, s"expected more than two items, found: ${all.size}")
      private[this] val ary = all.toArray

      override def parseMut(state: State): A = oneOf(ary, state)
    }

    case class StringIn(sorted: SortedSet[String]) extends Parser[Unit] {
      require(sorted.size >= 2, s"expected more than two items, found: ${sorted.size}")
      require(!sorted.contains(""), "empty string is not allowed in alternatives")
      private[this] val tree =
        RadixNode.fromSortedStrings(NonEmptyList.fromListUnsafe(sorted.toList))

      override def parseMut(state: State): Unit = stringIn(tree, sorted, state)
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

    case class Map0[A, B](parser: Parser0[A], fn: A => B) extends Parser0[B] {
      override def parseMut(state: State): B = Impl.map(parser, fn, state)
    }

    case class Map[A, B](parser: Parser[A], fn: A => B) extends Parser[B] {
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

    case class TailRecM0[A, B](init: A, fn: A => Parser0[Either[A, B]]) extends Parser0[B] {
      private[this] val p1 = fn(init)

      override def parseMut(state: State): B = Impl.tailRecM(p1, fn, state)
    }

    case class TailRecM[A, B](init: A, fn: A => Parser[Either[A, B]]) extends Parser[B] {
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

    /** capture parser p repeatedly, at least min times, at most max times
      */
    final def repCapture[A, B](
        p: Parser[A],
        min: Int,
        maxMinusOne: Int,
        state: State,
        append: Appender[A, B]
    ): Boolean = {
      var offset = state.offset
      var cnt = 0
      // maxMinusOne == Int.MaxValue is a sentinel value meaning "forever"
      while (cnt <= maxMinusOne) {
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
            // or didn't read at least min items
            // (todo: or cnt overflowed)
            return false
          }
        }
      }
      true
    }

    final def repNoCapture[A](p: Parser[A], min: Int, maxMinusOne: Int, state: State): Unit = {
      var offset = state.offset
      var cnt = 0
      // maxMinusOne == Int.MaxValue is a sentinel value meaning "forever"
      while (cnt <= maxMinusOne) {
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

    case class Rep0[A, B](p1: Parser[A], maxMinusOne: Int, acc: Accumulator0[A, B])
        extends Parser0[B] {
      private[this] val ignore: B = null.asInstanceOf[B]

      override def parseMut(state: State): B = {
        if (state.capture) {
          val app = acc.newAppender()
          if (repCapture(p1, 0, maxMinusOne, state, app)) app.finish()
          else ignore
        } else {
          repNoCapture(p1, 0, maxMinusOne, state)
          ignore
        }
      }
    }

    /** A parser that can repeats the underlying parser multiple times
      */
    case class Rep[A, B](p1: Parser[A], min: Int, maxMinusOne: Int, acc1: Accumulator[A, B])
        extends Parser[B] {
      if (min < 1) throw new IllegalArgumentException(s"expected min >= 1, found: $min")

      private[this] val ignore: B = null.asInstanceOf[B]

      override def parseMut(state: State): B = {
        // first parse one, so we can initialize the appender with that value
        // then do the rest, with min -> min - 1 and
        // maxMinusOne -> maxMinusOne - 1 or Int.MaxValue as "forever" sentinel
        val head = p1.parseMut(state)
        def maxRemainingMinusOne =
          if (maxMinusOne == Int.MaxValue) Int.MaxValue else maxMinusOne - 1
        if (state.error ne null) ignore
        else if (state.capture) {
          val app = acc1.newAppender(head)
          if (repCapture(p1, min - 1, maxRemainingMinusOne, state, app)) app.finish()
          else ignore
        } else {
          repNoCapture(p1, min - 1, maxRemainingMinusOne, state)
          ignore
        }
      }
    }

    /*
     * Merge CharIn bitsets
     */
    def mergeCharIn[A, P0 <: Parser0[A]](ps: List[P0]): List[P0] = {
      @annotation.tailrec
      def loop(ps: List[P0], front: List[CharIn], result: Chain[P0]): Chain[P0] = {
        @inline
        def frontRes: Chain[P0] =
          front match {
            case Nil => Chain.nil
            case one :: Nil => Chain.one(one.asInstanceOf[P0])
            case many =>
              // we need to union
              val minBs: List[(Int, BitSetUtil.Tpe)] = many.map { case CharIn(m, bs, _) => (m, bs) }
              Chain.one(Parser.charIn(BitSetUtil.union(minBs)).asInstanceOf[P0])
          }

        ps match {
          case Nil => result ++ frontRes
          case AnyChar :: tail =>
            // AnyChar is bigger than all subsequent CharIn:
            // and any direct prefix CharIns
            val tail1 = tail.filterNot(_.isInstanceOf[CharIn])
            (result :+ AnyChar.asInstanceOf[P0]) ++ Chain.fromSeq(tail1)
          case (ci: CharIn) :: tail =>
            loop(tail, ci :: front, result)
          case h :: tail =>
            // h is not an AnyChar or CharIn
            // we make our prefix frontRes
            // and resume working on the tail
            loop(tail, Nil, (result ++ frontRes) :+ h)
        }
      }

      loop(ps, Nil, Chain.nil).toList
    }

    /*
     * Merge Str and StringIn
     *
     * the semantic issue is this:
     * oneOf matches the first, left to right
     * StringIn matches the longest.
     * we can only merge into the left if
     * there are no prefixes of the right inside the left
     */
    def mergeStrIn[A, P0 <: Parser0[A]](ps: List[P0]): List[P0] = {
      @annotation.tailrec
      def loop(ps: List[P0], front: SortedSet[String], result: Chain[P0]): Chain[P0] = {
        @inline
        def res(front: SortedSet[String]): Chain[P0] =
          if (front.isEmpty) Chain.nil
          else if (front.size == 1) Chain.one(Str(front.head).asInstanceOf[P0])
          else Chain.one(StringIn(front).asInstanceOf[P0])

        // returns if there is a strict prefix (equality does not count)
        def frontHasPrefixOf(s: String): Boolean =
          front.exists { f => s.startsWith(f) && (f.length != s.length) }

        ps match {
          case Nil => result ++ res(front)
          case Str(s) :: tail =>
            if (frontHasPrefixOf(s)) {
              // there is an overlap, so we need to match what we have first, then come here
              loop(tail, SortedSet(s), result ++ res(front))
            } else {
              // there is no overlap in the tree, just merge it in:
              loop(tail, front + s, result)
            }
          case StringIn(ss) :: tail =>
            val (rights, lefts) = ss.partition(frontHasPrefixOf(_))
            val front1 = front | lefts
            if (rights.nonEmpty) {
              // there are some that can't be merged in
              loop(tail, rights, result ++ res(front1))
            } else {
              // everything can be merged in
              loop(tail, front1, result)
            }
          case h :: tail =>
            loop(tail, SortedSet.empty, (result ++ res(front)) :+ h)
        }
      }

      loop(ps, SortedSet.empty, Chain.nil).toList
    }

    case object AnyChar extends Parser[Char] {
      override def parseMut(state: State): Char = {
        val offset = state.offset
        if (offset < state.str.length) {
          val char = state.str.charAt(offset)
          state.offset += 1
          char
        } else {
          state.error =
            Eval.later(Chain.one(Expectation.InRange(offset, Char.MinValue, Char.MaxValue)))
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
            state.error = Eval.later(makeError(offset))
            '\u0000'
          }
        } else {
          state.error = Eval.later(makeError(offset))
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
          state.error = Eval.later(Chain.one(Expectation.ExpectedFailureAt(offset, matchedStr)))
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

    case class WithContextP0[A](context: String, under: Parser0[A]) extends Parser0[A] {
      override def parseMut(state: State): A = {
        val a = under.parseMut(state)
        if (state.error ne null) {
          state.error = state.error.map(_.map(Expectation.WithContext(context, _)))
        }
        a
      }
    }

    case class WithContextP[A](context: String, under: Parser[A]) extends Parser[A] {
      override def parseMut(state: State): A = {
        val a = under.parseMut(state)
        if (state.error ne null) {
          state.error = state.error.map(_.map(Expectation.WithContext(context, _)))
        }
        a
      }
    }
  }
}

//holds just the typeclass instances, and brings them in implicit scope
object Parser0 {
  implicit val catInstancesParser0
      : Monad[Parser0] with Alternative[Parser0] with Defer[Parser0] with FunctorFilter[Parser0] =
    new Monad[Parser0] with Alternative[Parser0] with Defer[Parser0] with FunctorFilter[Parser0] {
      override def pure[A](a: A): Parser0[A] = Parser.pure(a)

      override def defer[A](a: => Parser0[A]) = Parser.defer0(a)

      override def empty[A]: Parser0[A] = Parser.Fail

      override def functor = this

      override def map[A, B](fa: Parser0[A])(fn: A => B): Parser0[B] = Parser.map0(fa)(fn)

      override def mapFilter[A, B](fa: Parser0[A])(f: A => Option[B]): Parser0[B] =
        fa.mapFilter(f)

      override def replicateA[A](n: Int, fa: Parser0[A]): Parser0[List[A]] = {
        fa match {
          case p: Parser[A] if n >= 1 => Parser.repExactlyAs(p, n)
          case _ => super.replicateA(n, fa)
        }
      }

      override def filter[A](fa: Parser0[A])(fn: A => Boolean): Parser0[A] =
        fa.filter(fn)

      override def product[A, B](fa: Parser0[A], fb: Parser0[B]): Parser0[(A, B)] =
        Parser.product0(fa, fb)

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

      override def flatMap[A, B](fa: Parser0[A])(fn: A => Parser0[B]): Parser0[B] =
        Parser.flatMap0(fa)(fn)

      override def combineK[A](pa: Parser0[A], pb: Parser0[A]): Parser0[A] =
        Parser.oneOf0(pa :: pb :: Nil)

      override def tailRecM[A, B](init: A)(fn: A => Parser0[Either[A, B]]): Parser0[B] =
        Parser.tailRecM0(init)(fn)

      override def void[A](pa: Parser0[A]): Parser0[Unit] =
        Parser.void0(pa)

      override def as[A, B](pa: Parser0[A], b: B): Parser0[B] =
        Parser.as0(pa, b)

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
