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
package bench

import cats.parse.Parser
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
class StringInBenchmarks {
  val inputs =
    List("foofoo", "bar", "foobat", "foot", "foobar")

  val stringsToMatch =
    "foobar" :: "foofoo" :: "foobaz" :: "foo" :: "bar" :: Nil

  val radixNode = RadixNode.fromStrings(stringsToMatch)

  val stringIn = Parser.stringIn(stringsToMatch)

  val oneOf =
    Parser.oneOf(
      stringsToMatch.map { s => Parser.string(s) }
    )

  @Benchmark
  def stringInParse(): Unit =
    inputs.foreach(stringIn.parseAll(_))

  @Benchmark
  def oneOfParse(): Unit =
    inputs.foreach(oneOf.parseAll(_))

  @Benchmark
  def radixMatchIn(): Unit =
    inputs.foreach { s => radixNode.matchAt(s, 0) >= 0 }

  @Benchmark
  def linearMatchIn(): Unit =
    inputs.foreach { s => stringsToMatch.exists(s.startsWith(_)) }
}
