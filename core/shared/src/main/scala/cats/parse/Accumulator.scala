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

  def intCounter(): Appender[Any, Int] =
    new Appender[Any, Int] {
      private[this] var n = 0
      def append(item: Any) = {
        n += 1
        this
      }
      def finish(): Int = n
    }
}

/** Creates an appender given the first item to be added This is used to build the result in
  * Parser.repAs
  */
trait Accumulator[-A, +B] {
  def newAppender(first: A): Appender[A, B]
}

/** Creates an appender This is used to build the result in Parser.repAs0
  */
trait Accumulator0[-A, +B] extends Accumulator[A, B] {
  def newAppender(): Appender[A, B]
  def newAppender(first: A): Appender[A, B] =
    newAppender().append(first)
}

object Accumulator0 {
  implicit val intCounter0: Accumulator0[Any, Int] = new Accumulator0[Any, Int] {
    override def newAppender(): Appender[Any, Int] = Appender.intCounter()
  }

  implicit val charStringAccumulator0: Accumulator0[Char, String] =
    new Accumulator0[Char, String] {
      def newAppender() = Appender.charStringAppender()
    }

  implicit val stringAccumulator0: Accumulator0[String, String] =
    new Accumulator0[String, String] {
      def newAppender() = Appender.stringAppender()
    }

  implicit def listAccumulator0[A]: Accumulator0[A, List[A]] =
    new Accumulator0[A, List[A]] {
      def newAppender() = Appender.fromBuilder(List.newBuilder[A])
    }

  implicit def vectorAccumulator0[A]: Accumulator0[A, Vector[A]] =
    new Accumulator0[A, Vector[A]] {
      def newAppender() = Appender.fromBuilder(Vector.newBuilder[A])
    }

  /** An accumulator that does nothing and returns Unit Note, this should not generally be used with
    * repAs0 because internal allocations still happen. Instead use .rep0.void
    */
  val unitAccumulator0: Accumulator0[Any, Unit] =
    new Accumulator0[Any, Unit] {
      def newAppender() = Appender.unitAppender
    }
}

object Accumulator extends Priority0Accumulator {
  implicit def nonEmptyListAccumulator0[A]: Accumulator[A, NonEmptyList[A]] =
    new Accumulator[A, NonEmptyList[A]] {
      def newAppender(first: A): Appender[A, NonEmptyList[A]] =
        new Appender[A, NonEmptyList[A]] {
          val bldr = List.newBuilder[A]
          def append(item: A) = {
            bldr += item
            this
          }

          def finish() = NonEmptyList(first, bldr.result())
        }
    }

  implicit def nonEmptyVectorAccumulator0[A]: Accumulator[A, NonEmptyVector[A]] =
    new Accumulator[A, NonEmptyVector[A]] {
      def newAppender(first: A): Appender[A, NonEmptyVector[A]] =
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

private[parse] sealed trait Priority0Accumulator {
  implicit def fromAccumulator0[A, B](implicit acc: Accumulator0[A, B]): Accumulator[A, B] = acc
}
