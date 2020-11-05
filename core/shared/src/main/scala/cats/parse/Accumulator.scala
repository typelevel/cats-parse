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

import cats.data.{NonEmptyList, NonEmptyVector}
import scala.collection.mutable.Builder

/** A limited Builder-like value we use for portability
  */
trait Appender[-A, +B] {
  def append(item: A): this.type
  def finish(): B
}

object Appender {
  def charStringAppender(): Appender[Char, String] =
    new Appender[Char, String] {
      val bldr = new java.lang.StringBuilder()

      def append(item: Char) = {
        bldr.append(item)
        this
      }

      def finish(): String = bldr.toString()
    }

  def stringAppender(): Appender[String, String] =
    new Appender[String, String] {
      val bldr = new java.lang.StringBuilder()

      def append(item: String) = {
        bldr.append(item)
        this
      }

      def finish(): String = bldr.toString()
    }

  def fromBuilder[A, B](bldr: Builder[A, B]): Appender[A, B] =
    new Appender[A, B] {
      def append(item: A) = {
        bldr += item
        this
      }

      def finish(): B = bldr.result()
    }

  val unitAppender: Appender[Any, Unit] =
    new Appender[Any, Unit] {
      def append(item: Any) = this
      def finish(): Unit = ()
    }
}

/** Creates an appender given the first item to be added
  * This is used to build the result in Parser1.repAs1
  */
trait Accumulator1[-A, +B] {
  def newAppender(first: A): Appender[A, B]
}

/** Creates an appender
  * This is used to build the result in Parser1.repAs
  */
trait Accumulator[-A, +B] extends Accumulator1[A, B] {
  def newAppender(): Appender[A, B]
  def newAppender(first: A): Appender[A, B] =
    newAppender().append(first)
}

object Accumulator {
  implicit val charStringAccumulator: Accumulator[Char, String] =
    new Accumulator[Char, String] {
      def newAppender() = Appender.charStringAppender()
    }

  implicit val stringAccumulator: Accumulator[String, String] =
    new Accumulator[String, String] {
      def newAppender() = Appender.stringAppender()
    }

  implicit def listAccumulator[A]: Accumulator[A, List[A]] =
    new Accumulator[A, List[A]] {
      def newAppender() = Appender.fromBuilder(List.newBuilder[A])
    }

  implicit def vectorAccumulator[A]: Accumulator[A, Vector[A]] =
    new Accumulator[A, Vector[A]] {
      def newAppender() = Appender.fromBuilder(Vector.newBuilder[A])
    }

  /** An accumulator that does nothing and returns Unit
    * Note, this should not generally be used with repAs
    * because internal allocations still happen. Instead
    * use .rep.void
    */
  val unitAccumulator: Accumulator[Any, Unit] =
    new Accumulator[Any, Unit] {
      def newAppender() = Appender.unitAppender
    }
}

object Accumulator1 extends Priority0Accumulator1 {
  implicit def nonEmptyListAccumulator[A]: Accumulator1[A, NonEmptyList[A]] =
    new Accumulator1[A, NonEmptyList[A]] {
      def newAppender(first: A) =
        new Appender[A, NonEmptyList[A]] {
          val bldr = List.newBuilder[A]
          def append(item: A) = {
            bldr += item
            this
          }

          def finish() = NonEmptyList(first, bldr.result())
        }
    }

  implicit def nonEmptyVectorAccumulator[A]: Accumulator1[A, NonEmptyVector[A]] =
    new Accumulator1[A, NonEmptyVector[A]] {
      def newAppender(first: A) =
        new Appender[A, NonEmptyVector[A]] {
          val bldr = Vector.newBuilder[A]
          bldr += first

          def append(item: A) = {
            bldr += item
            this
          }

          def finish() = NonEmptyVector.fromVectorUnsafe(bldr.result())
        }
    }
}

private[parse] sealed trait Priority0Accumulator1 {
  implicit def fromAccumulator[A, B](implicit acc: Accumulator[A, B]): Accumulator1[A, B] = acc
}
