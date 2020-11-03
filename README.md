# cats-parse

[![Continuous Integration](https://github.com/typelevel/cats-parse/workflows/Continuous%20Integration/badge.svg)](https://github.com/typelevel/cats-parse/actions?query=workflow%3A%22Continuous+Integration%22)

A parsing library for the cats ecosystem.

To use in sbt add dependency to `"org.typelevel" %%% "cats-parse" % "0.0.1"`. The [API docs](https://oss.sonatype.org/service/local/repositories/releases/archive/org/typelevel/cats-parse_2.12/0.0.1/cats-parse_2.12-0.0.1-javadoc.jar/!/cats/parse/index.html) are published.

Why another parsing library? See this [blog post detailing the
design](https://posco.medium.com/designing-a-parsing-library-in-scala-d5076de52536). To reiterate,
this library has a few goals:

1. Compatability: should work on all scala platforms and recent versions. Currently it supports JVM, JS on versions 2.12, 2.13, and Dotty. The core library should have minimal dependencies. Currently this library only depends on cats.
2. Excellent performance: should be as fast or faster than any parser combinator that has comparable scala version support.
3. Cats friendliness: method names match cats style, and out of the box support for cats typeclasses.
4. Precise errors: following the [Haskell Trifecta parsing library](https://hackage.haskell.org/package/trifecta), backtracking is opt-in vs opt-out. This design tends to make it easier to write parsers that point correctly to failure points.
5. Safety: by introducing Parser1, a parser that must consume at least one character on success, some combinators and methods can be made safer to use and less prone to runtime errors.
6. Stability: we are very reluctant to break compatibility between versions. We want to put a minimal tax on users to stay on the latest versions.

# An Example

Below is most of a json parser (the string unescaping is elided). This example can give you a feel
for what it is like to use this library.

```scala
import cats.parse.{Parser => P, Parser1, Numbers}
import org.typelevel.jawn.ast._

object Json {
  private[this] val whitespace: Parser1[Unit] = P.charIn(" \t\r\n").void
  private[this] val whitespaces0: P[Unit] = whitespace.rep.void

  val parser: Parser1[JValue] = {
    val recurse = P.defer1(parser)
    val pnull = P.string1("null").as(JNull)
    val bool = P.string1("true").as(JBool.True).orElse1(P.string1("false").as(JBool.False))
    val justStr = JsonStringUtil.escapedString('"')
    val str = justStr.map(JString(_))
    val num = Numbers.jsonNumber.map(JNum(_))

    val listSep: Parser1[Unit] =
      (whitespaces0.with1.soft ~ P.char(',') ~ whitespaces0).void

    def rep[A](pa: Parser1[A]): P[List[A]] =
      (whitespaces0 *> P.repSep(pa, min = 0, sep = listSep) <* whitespaces0)

    val list = (P.char('[') *> rep(recurse) <* P.char(']'))
      .map { vs => JArray.fromSeq(vs) }

    val kv: Parser1[(String, JValue)] =
      justStr ~ ((whitespaces0.with1 ~ P.char(':') ~ whitespaces0) *> recurse)

    val obj = (P.char('{') *> rep(kv) <* P.char('}'))
      .map { vs => JObject.fromSeq(vs) }

    P.oneOf1(str :: num :: list :: obj :: bool :: pnull :: Nil)
  }

  // any whitespace followed by json followed by whitespace followed by end
  val parserFile: Parser1[JValue] = whitespaces0.with1 *> parser <* (whitespaces0 ~ P.end)
}
```

# Performance

We have a benchmark suite that compares JSON parsing across several commonly used libraries. A
recent (2020/11/02) result is below:

```
[info] Benchmark                        Mode  Cnt    Score    Error  Units
[info] BarBench.catsParseParse          avgt   10   ≈ 10⁻³           ms/op
[info] BarBench.fastparseParse          avgt   10   ≈ 10⁻⁴           ms/op
[info] BarBench.jawnParse               avgt   10   ≈ 10⁻⁴           ms/op
[info] BarBench.parboiled2Parse         avgt   10   ≈ 10⁻⁴           ms/op
[info] BarBench.parsleyParseCold        avgt   10    0.083 ±  0.001  ms/op
[info] Bla25Bench.catsParseParse        avgt   10   34.558 ±  0.192  ms/op
[info] Bla25Bench.fastparseParse        avgt   10   21.651 ±  0.050  ms/op
[info] Bla25Bench.jawnParse             avgt   10    9.758 ±  0.059  ms/op
[info] Bla25Bench.parboiled2Parse       avgt   10   29.403 ±  0.082  ms/op
[info] Bla25Bench.parsleyParseCold      avgt   10   38.594 ±  0.220  ms/op
[info] CountriesBench.catsParseParse    avgt   10   12.656 ±  0.031  ms/op
[info] CountriesBench.fastparseParse    avgt   10    6.722 ±  0.040  ms/op
[info] CountriesBench.jawnParse         avgt   10    1.720 ±  0.004  ms/op
[info] CountriesBench.parboiled2Parse   avgt   10    5.674 ±  0.018  ms/op
[info] CountriesBench.parsleyParseCold  avgt   10   16.040 ±  0.070  ms/op
[info] Qux2Bench.catsParseParse         avgt   10   10.692 ±  0.040  ms/op
[info] Qux2Bench.fastparseParse         avgt   10    9.195 ±  0.074  ms/op
[info] Qux2Bench.jawnParse              avgt   10    2.792 ±  0.014  ms/op
[info] Qux2Bench.parboiled2Parse        avgt   10    8.641 ±  0.034  ms/op
[info] Qux2Bench.parsleyParseCold       avgt   10   14.539 ±  0.222  ms/op
[info] Ugh10kBench.catsParseParse       avgt   10   87.602 ±  0.554  ms/op
[info] Ugh10kBench.fastparseParse       avgt   10   63.500 ±  0.144  ms/op
[info] Ugh10kBench.jawnParse            avgt   10   16.802 ±  0.082  ms/op
[info] Ugh10kBench.parboiled2Parse      avgt   10   49.871 ±  0.161  ms/op
[info] Ugh10kBench.parsleyParseCold     avgt   10  104.814 ±  0.460  ms/op
```

Note that parboiled and fastparse both use macros that make them very difficult to port to Dotty.
Jawn is a specialized and optimized JSON parser, so that can be considered an upper bound on
performance.
Keep in mind that parser performance depends both on the parsing library but also how the parser
is written, but these results suggest that this library is already quite competitive.

# Migrating from Fastparse
You should find all the Fastparse methods you are used to. If not, feel free to open an issue.
There are a few things to keep in mind:

1. In fastparse, you wrap a parser in `P(...)` to make the interior lazy. Following cats, to get a lazily constructed parser use `Parser.defer` or `cats.Defer[Parser].defer`.
2. In fastparse the `~` operator does tuple concatenation. This can be nice, but also complex to see what the resulting type is. In cats-parse, `~` always returns a Tuple2 containing the parsed values from the left and right. To recover fastparse-like behavior, use cats syntax `(pa, pb, pc...).tupled`.
3. In fastparse, backtracking is opt-out by using cuts. In cats-parse, backtracking is opt-in using `.backtrack`. Put another way, normal product operations in cats-parse are like `~/` in fastparse.
4. In cats-parse, using `*>`, `<*`, and `.void` methods can be a significant optimization: if you don't need a result, communicate that to the library with those methods.

# Getting and Giving Help

We welcome new contributors and new maintainers. Please feel free to open issues and PRs. If you have any
problem using the library, an issue is the best way to ask a question until we flush out more
documentation.
