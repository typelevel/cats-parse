# cats-parse

[![Continuous Integration](https://github.com/typelevel/cats-parse/workflows/Continuous%20Integration/badge.svg)](https://github.com/typelevel/cats-parse/actions?query=workflow%3A%22Continuous+Integration%22)[![codecov](https://codecov.io/gh/typelevel/cats-parse/branch/main/graph/badge.svg)](https://codecov.io/gh/typelevel/cats-parse)

A parsing library for the cats ecosystem.

To use in sbt add, the following to your `libraryDependencies`:

```scala
// use this snippet for the JVM
libraryDependencies += "org.typelevel" %% "cats-parse" % "0.3.7"

// use this snippet for JS, or cross-building
libraryDependencies += "org.typelevel" %%% "cats-parse" % "0.3.7"
```

The [API docs](https://javadoc.io/doc/org.typelevel/cats-parse_2.13/0.3.7/cats/parse/index.html) are published.

Why another parsing library? See this [blog post detailing the
design](https://posco.medium.com/designing-a-parsing-library-in-scala-d5076de52536). To reiterate,
this library has a few goals:

1. Compatibility: should work on all scala platforms and recent versions. Currently it supports JVM, JS on versions 2.11, 2.12, 2.13, and 3. The core library should have minimal dependencies. Currently this library only depends on cats.
2. Excellent performance: should be as fast or faster than any parser combinator that has comparable scala version support.
3. Cats friendliness: method names match cats style, and out of the box support for cats typeclasses.
4. Precise errors: following the [Haskell Trifecta parsing library](https://hackage.haskell.org/package/trifecta), backtracking is opt-in vs opt-out. This design tends to make it easier to write parsers that point correctly to failure points.
5. Safety: by separating Parser0, a parser that may consume no input, from Parser, a parser must consume at least one character on success. Most combinators and methods can be made safer to use and less prone to runtime errors.
6. Stability: we are very reluctant to break compatibility between versions. We want to put a minimal tax on users to stay on the latest versions.

# Tutorial

## Simple parser

The library provides a set of simple parsers which might be combined to create any parsing logic. The simplest parser is `Parser.anyChar` which is successful where there is one char at the input. It has type `Parser[Char]` which means it returns one parsed char.

To provide any input to parser one need to use `parse` method.

```scala mdoc:reset
import cats.parse.Parser

val p: Parser[Char] = Parser.anyChar

p.parse("t")
// res0: Either[Error, Tuple2[String, Char]] = Right((,t))
p.parse("")
// res1: Either[Error, Tuple2[String, Char]] = Left(Error(0,NonEmptyList(InRange(0,,))))
p.parse("two")
// res2: Either[Error, Tuple2[String, Char]] = Right((wo,t))
```

Notice the return type. `Tuple2[String, Char]` contains the rest of the input string and one parsed char if parsing was successful. It returns `Left` with error message if there was some parsing error.

## Mapping output

The output of the parser might be processed with `map` method:

```scala mdoc:reset
import cats.parse.Parser

case class CharWrapper(value: Char)

val p: Parser[CharWrapper] = Parser.anyChar.map(char => CharWrapper(char))

p.parse("t")
// res0 = Right((,CharWrapper(t)))
```

There are built-in methods for mapping the output to types `String` or `Unit`:

```scala mdoc:reset
import cats.parse.Rfc5234.digit
import cats.parse.Parser

/* String */

val p2: Parser[String] = digit.map((c: Char) => c.toString)
// is analog to
val p3: Parser[String] = digit.string

p3.parse("1")
// res0: Either[Error, Tuple2[String, String]] = Right((,1))

/* Unit */

val p4: Parser[Unit] = digit.map(_ => ())
// is analog to
val p5: Parser[Unit] = digit.void

p5.parse("1")
// res1: Either[Error, Tuple2[String, Unit]] = Right((,()))
```

## Combining parsers

The parsers might be combined through operators:

- `~` - product. Allows continuing parsing if the left side was successful;
- `<*`, `*>` - productL and productR. Works just like product but drop part of result;
- `surroundedBy` - identical to `border *> parsingResult <* border`;
- `between` - identical to `border1 *> parsingResult <* border2`;
- `|`, `orElse`. Parser will be successful if any of sides is successful.

For this example we'll be using `cats.parse.Rfc5234` package which contains such parsers as `alpha` (Latin alphabet) and `sp` (whitespace).

```scala mdoc:reset
import cats.parse.Rfc5234.{sp, alpha, digit}
import cats.parse.Parser

/* Product */

// the sp parser won't return the whitespace, it just returns Unit if it successful
val p1: Parser[(Char, Unit)] = alpha ~ sp

p1.parse("t")
// res0: Either[Error, Tuple2[String, Tuple2[Char, Unit]]] = Left(Error(1,NonEmptyList(InRange(1, , ))))
p1.parse("t ")
// res1: Either[Error, Tuple2[String, Tuple2[Char, Unit]]] = Right((,(t,())))

/* productL, productR */

// The type is just Char because we dropping the space
// to drop the alphabet change the arrow side: alpha *> sp
val p2: Parser[Char] = alpha <* sp

// still error since we need the space even if we drop it
p2.parse("t")
// res2: Either[Error, Tuple2[String, Char]] = Left(Error(1,NonEmptyList(InRange(1, , ))))
p2.parse("t ")
// res3: Either[Error, Tuple2[String, Char]] = Right((,t))

/* surroundedBy */

val p4: Parser[Char] = sp *> alpha <* sp
val p5: Parser[Char] = alpha.surroundedBy(sp)

p4.parse(" a ")
// res0: Either[Error, Tuple2[String, Char]] = Right((,a))
p5.parse(" a ")
// res1: Either[Error, Tuple2[String, Char]] = Right((,a))

/* between */

val p6: Parser[Char] = sp *> alpha <* digit
val p7: Parser[Char] = alpha.between(sp, digit)

p6.parse(" a1")
// res2: Either[Error, Tuple2[String, Char]] = Right((,a))
p7.parse(" a1")
// res3: Either[Error, Tuple2[String, Char]] = Right((,a))

/* OrElse */

val p3: Parser[AnyVal] = alpha | sp

p3.parse("t")
// res4: Either[Error, Tuple2[String, AnyVal]] = Right((,t))
p3.parse(" ")
// res5: Either[Error, Tuple2[String, AnyVal]] = Right((,()))
```

## Repeating parsers

Sometimes we need something to repeat zero or more types. The cats-parse have `rep` and `rep0` methods for repeating values. `rep` means that the parser must be successful _at least one time_. `rep0` means that the parser output might be empty.

```scala mdoc:reset
import cats.data.NonEmptyList
import cats.parse.Rfc5234.alpha
import cats.parse.{Parser, Parser0}

val p1: Parser[NonEmptyList[Char]]  = alpha.rep
val p2: Parser0[List[Char]] = alpha.rep0

p1.parse("")
// Left(Error(0,NonEmptyList(InRange(0,A,Z), InRange(0,a,z))))
p2.parse("")
// Right((,List()))
p2.parse("something")
// Right((,List(s, o, m, e, t, h, i, n, g)))
```

Notice the types of parsers. `Parser` type always means some non-empty output and the output of `Parser0` might be empty.

One common task in this example is to parse a full line (or words) of text. In the example it is done by `rep`, and then it could be mapped to `String` in different ways:

```scala mdoc:reset
import cats.data.NonEmptyList
import cats.parse.Rfc5234.alpha
import cats.parse.Parser

val p: Parser[String]  = alpha.rep.map((l: NonEmptyList[Char]) => l.toList.mkString)

val p2: Parser[String] = alpha.rep.string
val p3: Parser[String] = alpha.repAs[String]
```

All three parsers will be identical in parsing results, but `p2` and `p3` are using built-in methods which will not create intermediate list. `rep` + `map` creates intermediate list which is mapped to string in this example.

## Parsers with empty output

Some parsers never return a value. They have a type `Parser0`. One might get this type of parser when using `rep0` or `.?` methods.

```scala mdoc:reset
import cats.parse.Rfc5234.{alpha, sp}
import cats.parse.Parser

val p: Parser[String] = (alpha.rep <* sp.?).rep.string

p.parse("hello world")
// res0 = Right((,hello world))
```

Notice the type we got - `Parser[String]`. That is because we have `rep` outside and our `alpha.rep` parser with `Parser` type is on the left side of the clause. But what if we want to parse strings with spaces at the beginning?

```scala:fail
val p = (sp.? *> alpha.rep <* sp.?).rep.string
```

We will get an error `value rep is not a member of cats.parse.Parser0`. This happens since we have the left-side parser as optional in `sp.? *> alpha.rep <* sp.?` clause. This clause has a type `Parser0` which can't be repeated.

But this parser can't be empty because of `alpha.rep` parser, and we know it. For these types of parsers we need to use `with1` wrapper method on the _left side_ of the clause:

```scala mdoc:reset
import cats.parse.Rfc5234.{alpha, sp}
import cats.parse.Parser


val p: Parser[String] = (sp.?.with1 *> alpha.rep <* sp.?).rep.string

p.parse("hello world")
// res0: Either[Error, Tuple2[String, String]] = Right((,hello world))
p.parse(" hello world")
// res1: Either[Error, Tuple2[String, String]] = Right((,hello world))
```

If we have multiple `Parser0` parsers before the `Parser` - we'd need to use parenthesis like this:
`(sp.? ~ sp.?).with1 *> alpha.rep`.

## Error handling

Parser might be interrupted by parsing error. There are two kinds of errors:

- an error that has consumed 0 characters (**epsilon failure**);
- an error that has consumed 1 or more characters (**arresting failure**) (sometimes called halting failure).

```scala mdoc:reset
import cats.parse.Rfc5234.{alpha, sp}
import cats.parse.Parser

val p1: Parser[Char] = alpha
val p2: Parser[Char] = sp *> alpha

// epsilon failure
p1.parse("123")
// res0: Either[Error, Tuple2[String, Char]] = Left(Error(0,NonEmptyList(InRange(0,A,Z), InRange(0,a,z))))

// arresting failure
p2.parse(" 1")
// res1: Either[Error, Tuple2[String, Char]] = Left(Error(1,NonEmptyList(InRange(1,A,Z), InRange(1,a,z))))
```

We need to make this difference because the first type of error allows us to say that parser is not matching the input before we started to process it and the second error happens while parser processing the input.

### Backtrack

Backtrack allows us to convert an _arresting failure_ to _epsilon failure_. It also rewinds the input to the offset that was used before parsing began. The resulting parser might still be combined with others. Let's look at the example:

```scala mdoc:reset
import cats.parse.Rfc5234.{digit, sp}

val p = sp *> digit <* sp

p.parse(" 1")
// res1 = Left(Error(2,NonEmptyList(InRange(2, , ))))
```

`Parser.Error` contains two parameters:

```scala
final case class Error(input: String, failedAtOffset: Int, expected: NonEmptyList[Expectation])

case class InRange(offset: Int, lower: Char, upper: Char) extends Expectation
```

In the error message we see the failed offset and the expected value. There is a lot of expected error types which can be found in source code.

One thing we can do in this situation is providing a fallback parser which can be used in case of error. We can do this by using `backtrack` (which rewinds the input, so it will be passed to fallback parser as it was before the error) and combining it with `orElse` operator:

```scala mdoc:reset
import cats.parse.Rfc5234.{digit, sp}

val p1 = sp *> digit <* sp
val p2 = sp *> digit

p1.backtrack.orElse(p2).parse(" 1")
// res0: Either[Error, Tuple2[String, Char]] = Right((,1))
(p1.backtrack | p2).parse(" 1")
// res1: Either[Error, Tuple2[String, Char]] = Right((,1))
```

Notice that `(p1.backtrack | p2)` clause is another parser by itself since we're still combining parsers by using `orElse`.

But we've already used `orElse` in example before without any `backtrack` operator, and it worked just fine. Why do we need `backtrack` now? Let's look at this example:

```scala mdoc:reset
import cats.parse.Rfc5234.{digit, sp}

val p1 = sp *> digit <* sp
val p2 = sp *> digit
val p3 = digit

(p1 | p2).parse(" 1")
// res1 = Left(Error(2,NonEmptyList(InRange(2, , ))))

(p1 | p2 | p3).parse("1")
// res2 = Right((,1))
```

The first parser combination is interrupted by _arresting failures_ and the second parsing combination will only suffer from _epsilon failures_. The second parser works because `orElse` and `|` operators actually allows recovering from epsilon failures, but not from arresting failures.

So the `backtrack` helps us where the _left side_ returns arresting failure.

### Soft

This method might look similar to `backtrack`, but it allows us to _proceed_ the parsing when the _right side_ is returning an epsilon failure. It is really useful for ambiguous parsers when we can't really tell what exactly we are parsing before the end. Let's say we want to parse some input to the search engine which contains fields. This might look like "field:search_query". Let's try to write a parser for this:

```scala mdoc:reset
import cats.parse.Rfc5234.{alpha, sp}
import cats.parse.Parser
import cats.parse.Parser.{char => pchar}

val searchWord = alpha.rep.string

val fieldValue = alpha.rep.string ~ pchar(':')

val p1 = fieldValue.? ~ (searchWord ~ sp.?).rep.string


p1.parse("title:The Wind Has Risen")
// res0 = Right((,(Some((title,())),The Wind Has Risen)))
p1.parse("The Wind Has Risen")
// res1 = Left(Error(3,NonEmptyList(InRange(3,:,:))))
```

This error happens because we can't really tell if we are parsing the `fieldValue` before we met a `:` char. We might do this with by writing two parsers, converting the first one's failure to epsilon failure by `backtrack` and then providing fallback parser by `|` operator (which allows the epsilon failures):

```scala mdoc
val p2 = (searchWord ~ sp.?).rep.string

(p1.backtrack | p2).parse("title:The Wind Has Risen")
// res0 = Right((,(Some((title,())),The Wind Has Risen)))
(p1.backtrack | p2).parse("The Wind Has Risen")
// res1 = Right((,The Wind Has Risen))
```

But this problem might be resolved with `soft` method inside the first parser since the right side of it actually returns an epsilon failure itself:

```scala mdoc
val fieldValueSoft = alpha.rep.string.soft ~ pchar(':')

val p3 = fieldValueSoft.? ~ (searchWord ~ sp.?).rep.string

p3.parse("title:The Wind Has Risen")
// res2 = Right((,(Some((title,())),The Wind Has Risen)))
p3.parse("The Wind Has Risen")
// res3 = Right((,(None,The Wind 22Has Risen)))
```

So when the _right side_ returns an epsilon failure the `soft` method allows us to rewind parsed input and try to proceed it's parsing with next parsers (without changing the parser itself!).

Another common use case for `soft` is implementing a parser to find values interspersed with a separator. For example, if you want to extract `'a'`, `'b'` and `'c'` from `"a,b,c"`, you can use `soft`.

Naively, one may try the following:

```scala mdoc
val p4 = alpha
val naiveInterspersed = (p4 <* pchar(',')).rep
naiveInterspersed.parse("a,b,c")
// res4 = Left(Error(5,NonEmptyList(InRange(offset = 5, lower = ',', upper = ','),List())))
```

Basically, it's looking for the trailing comma:

```scala mdoc
naiveInterspersed.parse("a,b,c,")
// res5 = Right(("", NonEmptyList('a', List('b', 'c'))))
```

But you can use, `soft` along with `|` to implement that:
```scala mdoc
val interspersed = ((p4.soft <* pchar(',')) | p4).rep
interspersed.parse("a,b,c")
// res6 = Right(("", NonEmptyList('a', List('b', 'c'))))
```

# JSON parser example

Below is most of a json parser (the string unescaping is elided). This example can give you a feel
for what it is like to use this library.

```scala mdoc:invisible
import cats.parse.strings.Json.delimited.{parser => jsonString}
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
    val str = jsonString.map(JString(_))
    val num = Numbers.jsonNumber.map(JNum(_))

    val listSep: P[Unit] =
      P.char(',').soft.surroundedBy(whitespaces0).void

    def rep[A](pa: P[A]): Parser0[List[A]] =
      pa.repSep0(listSep).surroundedBy(whitespaces0)

    val list = rep(recurse).with1
      .between(P.char('['), P.char(']'))
      .map { vs => JArray.fromSeq(vs) }

    val kv: P[(String, JValue)] =
      jsonString ~ (P.char(':').surroundedBy(whitespaces0) *> recurse)

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
