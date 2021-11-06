# cats-parse

[![Continuous Integration](https://github.com/typelevel/cats-parse/workflows/Continuous%20Integration/badge.svg)](https://github.com/typelevel/cats-parse/actions?query=workflow%3A%22Continuous+Integration%22)[![codecov](https://codecov.io/gh/typelevel/cats-parse/branch/main/graph/badge.svg)](https://codecov.io/gh/typelevel/cats-parse)

A parsing library for the cats ecosystem.

To use in sbt add, the following to your `libraryDependencies`: 

```scala
// use this snippet for the JVM
libraryDependencies += "org.typelevel" %% "cats-parse" % "0.3.4"

// use this snippet for JS, or cross-building
libraryDependencies += "org.typelevel" %%% "cats-parse" % "0.3.4"
```

The [API docs](https://oss.sonatype.org/service/local/repositories/releases/archive/org/typelevel/cats-parse_2.13/0.3.4/cats-parse_2.13-0.3.4-javadoc.jar/!/cats/parse/index.html) are published.

Why another parsing library? See this [blog post detailing the
design](https://posco.medium.com/designing-a-parsing-library-in-scala-d5076de52536). To reiterate,
this library has a few goals:

1. Compatability: should work on all scala platforms and recent versions. Currently it supports JVM, JS on versions 2.12, 2.13, and Dotty. The core library should have minimal dependencies. Currently this library only depends on cats.
2. Excellent performance: should be as fast or faster than any parser combinator that has comparable scala version support.
3. Cats friendliness: method names match cats style, and out of the box support for cats typeclasses.
4. Precise errors: following the [Haskell Trifecta parsing library](https://hackage.haskell.org/package/trifecta), backtracking is opt-in vs opt-out. This design tends to make it easier to write parsers that point correctly to failure points.
5. Safety: by separating Parser0, a parser that may consume no input, from Parser, a parser must consume at least one character on success. Most combinators and methods can be made safer to use and less prone to runtime errors.
6. Stability: we are very reluctant to break compatibility between versions. We want to put a minimal tax on users to stay on the latest versions.

# An Example

Below is most of a json parser (the string unescaping is elided). This example can give you a feel
for what it is like to use this library.

```scala mdoc:invisible
import cats.parse.bench.self.JsonStringUtil
```

```scala mdoc
import cats.parse.{Parser0, Parser => P, Numbers}
import org.typelevel.jawn.ast._

object Json {
  private[this] val whitespace: P[Unit] = P.charIn(" \t\r\n").void
  private[this] val whitespaces0: Parser0[Unit] = whitespace.rep0.void

  val parser: P[JValue] = P.recursive[JValue] { recurse =>
    val pnull = P.string("null").as(JNull)
    val bool = P.string("true").as(JBool.True).orElse(P.string("false").as(JBool.False))
    val justStr = JsonStringUtil.escapedString('"')
    val str = justStr.map(JString(_))
    val num = Numbers.jsonNumber.map(JNum(_))

    val listSep: P[Unit] =
      P.char(',').soft.surroundedBy(whitespaces0).void

    def rep[A](pa: P[A]): Parser0[List[A]] =
      pa.repSep0(listSep).surroundedBy(whitespaces0)

    val list = rep(recurse).with1
      .between(P.char('['), P.char(']'))
      .map { vs => JArray.fromSeq(vs) }

    val kv: P[(String, JValue)] =
      justStr ~ (P.char(':').surroundedBy(whitespaces0) *> recurse)

    val obj = rep(kv).with1
      .between(P.char('{'), P.char('}'))
      .map { vs => JObject.fromSeq(vs) }

    P.oneOf(str :: num :: list :: obj :: bool :: pnull :: Nil)
  }

  // any whitespace followed by json followed by whitespace followed by end
  val parserFile: P[JValue] = whitespaces0.with1 *> parser <* (whitespaces0 ~ P.end)
}
```

# Performance

We have a benchmark suite that compares JSON parsing across several commonly used libraries. A
recent (2021/11/05) result is below:

```
[info] Benchmark                         Mode  Cnt    Score    Error  Units
[info] BarBench.catsParseParse           avgt    4   ≈ 10⁻⁴           ms/op
[info] BarBench.fastparseParse           avgt    4   ≈ 10⁻⁴           ms/op
[info] BarBench.jawnParse                avgt    4   ≈ 10⁻⁴           ms/op
[info] BarBench.parboiled2Parse          avgt    4   ≈ 10⁻⁴           ms/op
[info] BarBench.parsleyParseCold         avgt    4    0.064 ±  0.001  ms/op
[info] Bla25Bench.catsParseParse         avgt    4   23.095 ±  0.174  ms/op
[info] Bla25Bench.fastparseParse         avgt    4   15.622 ±  0.414  ms/op
[info] Bla25Bench.jawnParse              avgt    4    7.501 ±  0.143  ms/op
[info] Bla25Bench.parboiled2Parse        avgt    4   18.423 ±  6.094  ms/op
[info] Bla25Bench.parsleyParseCold       avgt    4   30.752 ±  0.279  ms/op
[info] CountriesBench.catsParseParse     avgt    4    7.169 ±  0.041  ms/op
[info] CountriesBench.fastparseParse     avgt    4    5.023 ±  0.023  ms/op
[info] CountriesBench.jawnParse          avgt    4    1.235 ±  0.011  ms/op
[info] CountriesBench.parboiled2Parse    avgt    4    2.936 ±  0.008  ms/op
[info] CountriesBench.parsleyParseCold   avgt    4   11.800 ±  0.162  ms/op
[info] Qux2Bench.catsParseParse          avgt    4    7.031 ±  0.599  ms/op
[info] Qux2Bench.fastparseParse          avgt    4    6.597 ±  0.031  ms/op
[info] Qux2Bench.jawnParse               avgt    4    2.227 ±  0.014  ms/op
[info] Qux2Bench.parboiled2Parse         avgt    4    5.514 ±  0.472  ms/op
[info] Qux2Bench.parsleyParseCold        avgt    4   10.327 ±  0.293  ms/op
[info] StringInBenchmarks.oneOfParse     avgt    4   88.105 ±  2.658  ns/op
[info] StringInBenchmarks.stringInParse  avgt    4  129.246 ±  1.820  ns/op
[info] Ugh10kBench.catsParseParse        avgt    4   53.679 ±  1.385  ms/op
[info] Ugh10kBench.fastparseParse        avgt    4   45.165 ±  0.356  ms/op
[info] Ugh10kBench.jawnParse             avgt    4   11.404 ±  0.068  ms/op
[info] Ugh10kBench.parboiled2Parse       avgt    4   31.984 ±  0.748  ms/op
[info] Ugh10kBench.parsleyParseCold      avgt    4   77.150 ±  1.093  ms/op
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
