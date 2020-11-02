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

package cats.parse.bench

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import org.typelevel.jawn.ast.JValue
import scala.io.Source

/* Based on https://github.com/typelevel/jawn/blob/v1.0.0/benchmark/src/main/scala/jawn/JmhBenchmarks.scala */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
abstract class JmhBenchmarks(name: String) {
  val text: String =
    Source.fromResource(name, getClass.getClassLoader).getLines().mkString("\n")

  // @Benchmark too slow
  def attoParse(): JValue = {
    import _root_.atto._, Atto._
    cats.parse.bench.atto.JsonExample.jexpr.parseOnly(text).option.get
  }

  @Benchmark
  def catsParseParse(): JValue =
    self.Json.parser.parse(text) match {
      case Right((_, json)) => json
      case Left(e) => sys.error(e.toString)
    }

  @Benchmark
  def fastparseParse(): JValue =
    _root_.fastparse.parse(text, fastparse.JsonParse.jsonExpr(_)).get.value

  @Benchmark
  def jawnParse(): JValue =
    org.typelevel.jawn.ast.JParser.parseFromString(text).get

  @Benchmark
  def parboiled2Parse(): JValue =
    new parboiled2.JsonParser(text).Json.run().get

  @Benchmark
  def parsleyParseCold(): JValue =
    org.http4s.parsley.runParserThreadSafe(parsley.ParsleyJson.json, text).toOption.get

  // Stable instance to warm up
  val hotParsley = parsley.ParsleyJson.json

  // @Benchmark Failing when multithreaded
  def parsleyParseHot(): JValue =
    org.http4s.parsley.runParserThreadSafe(hotParsley, text).toOption.get
}

class BarBench extends JmhBenchmarks("bar.json")
class Qux2Bench extends JmhBenchmarks("qux2.json")
class Bla25Bench extends JmhBenchmarks("bla25.json")
class CountriesBench extends JmhBenchmarks("countries.geo.json")
class Ugh10kBench extends JmhBenchmarks("ugh10k.json")
