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

import cats.{Eq, Id, FlatMap, Functor, Defer, MonoidK, Monad, Eval}
import cats.arrow.FunctionK
import cats.data.NonEmptyList
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Cogen}

import cats.implicits._

sealed abstract class GenT[F[_]] { self =>
  type A
  val cogen: Cogen[A]
  val fa: F[A]

  def transform[G[_]](fk: FunctionK[F, G]): GenT[G] =
    new GenT[G] {
      type A = self.A
      val cogen = self.cogen
      val fa: G[A] = fk(self.fa)
    }

  def toId(implicit F: Functor[F]): F[GenT[Id]] =
    F.map(fa) { a =>
      new GenT[Id] {
        type A = self.A
        val cogen = self.cogen
        val fa = a
      }
    }

  override def toString: String = s"GenT($fa)"
}

object GenT {
  def apply[F[_], A0: Cogen](pa: F[A0]): GenT[F] =
    new GenT[F] {
      type A = A0
      val cogen = implicitly[Cogen[A0]]
      val fa = pa
    }
}

object ParserGen {
  implicit val functorGen: Functor[Gen] =
    new Functor[Gen] {
      def map[A, B](ga: Gen[A])(fn: A => B) = ga.map(fn)
    }

  def arbGen[A: Arbitrary: Cogen]: GenT[Gen] =
    GenT(Arbitrary.arbitrary[A])

  val pures: Gen[GenT[Gen]] =
    Gen.oneOf(arbGen[Int], arbGen[Boolean], arbGen[String], arbGen[(Int, Int)])

  val expect0: Gen[GenT[Parser0]] =
    Arbitrary.arbitrary[String].map { str =>
      GenT(Parser0.string0(str))
    }

  val ignoreCase0: Gen[GenT[Parser0]] =
    Arbitrary.arbitrary[String].map { str =>
      GenT(Parser0.ignoreCase0(str))
    }

  val charIn0: Gen[GenT[Parser0]] =
    Gen.oneOf(
      Arbitrary.arbitrary[List[Char]].map { cs =>
        GenT(Parser0.charIn(cs): Parser0[Char])
      },
      Gen.const(GenT(Parser0.anyChar: Parser0[Char]))
    )

  val charIn: Gen[GenT[Parser]] =
    Gen.oneOf(
      Arbitrary.arbitrary[List[Char]].map { cs =>
        GenT(Parser0.charIn(cs))
      },
      Gen.const(GenT(Parser0.anyChar))
    )

  val expect1: Gen[GenT[Parser]] =
    Arbitrary.arbitrary[String].map { str =>
      if (str.isEmpty) GenT(Parser0.fail: Parser[Unit])
      else GenT(Parser0.string(str))
    }

  val ignoreCase: Gen[GenT[Parser]] =
    Arbitrary.arbitrary[String].map { str =>
      if (str.isEmpty) GenT(Parser0.fail: Parser[Unit])
      else GenT(Parser0.ignoreCase(str))
    }

  val fail: Gen[GenT[Parser0]] =
    Gen.const(GenT(Parser0.fail: Parser0[Unit]))

  val failWith: Gen[GenT[Parser0]] =
    Arbitrary.arbitrary[String].map { str =>
      GenT(Parser0.failWith[Unit](str))
    }

  def void0(g: GenT[Parser0]): GenT[Parser0] =
    GenT(Parser0.void0(g.fa))

  def void(g: GenT[Parser]): GenT[Parser] =
    GenT(Parser0.void(g.fa))

  def string0(g: GenT[Parser0]): GenT[Parser0] =
    GenT(Parser0.string0(g.fa))

  def string(g: GenT[Parser]): GenT[Parser] =
    GenT(Parser0.string(g.fa))

  def backtrack0(g: GenT[Parser0]): GenT[Parser0] =
    GenT(g.fa.backtrack)(g.cogen)

  def backtrack(g: GenT[Parser]): GenT[Parser] =
    GenT(g.fa.backtrack)(g.cogen)

  def defer0(g: GenT[Parser0]): GenT[Parser0] =
    GenT(Defer[Parser0].defer(g.fa))(g.cogen)

  def defer(g: GenT[Parser]): GenT[Parser] =
    GenT(Defer[Parser].defer(g.fa))(g.cogen)

  def rep0(g: GenT[Parser]): GenT[Parser0] = {
    implicit val cg = g.cogen
    GenT[Parser0, List[g.A]](g.fa.rep0)
  }

  def rep(g: GenT[Parser]): GenT[Parser] = {
    implicit val cg = g.cogen
    GenT[Parser, List[g.A]](g.fa.rep.map(_.toList))
  }

  def product(ga: GenT[Parser0], gb: GenT[Parser0]): Gen[GenT[Parser0]] = {
    implicit val ca: Cogen[ga.A] = ga.cogen
    implicit val cb: Cogen[gb.A] = gb.cogen
    Gen.oneOf(
      GenT[Parser0, (ga.A, gb.A)](FlatMap[Parser0].product(ga.fa, gb.fa)),
      GenT[Parser0, (ga.A, gb.A)](FlatMap[Parser0].map2(ga.fa, gb.fa)((_, _))),
      GenT[Parser0, (ga.A, gb.A)](FlatMap[Parser0].map2Eval(ga.fa, Eval.later(gb.fa))((_, _)).value),
      GenT[Parser0, (ga.A, gb.A)](FlatMap[Parser0].map2Eval(ga.fa, Eval.now(gb.fa))((_, _)).value)
    )
  }

  def softProduct(ga: GenT[Parser0], gb: GenT[Parser0]): Gen[GenT[Parser0]] = {
    implicit val ca: Cogen[ga.A] = ga.cogen
    implicit val cb: Cogen[gb.A] = gb.cogen
    Gen.const(
      GenT[Parser0, (ga.A, gb.A)](ga.fa.soft ~ gb.fa)
    )
  }

  def product1(ga: GenT[Parser], gb: GenT[Parser]): Gen[GenT[Parser]] = {
    implicit val ca: Cogen[ga.A] = ga.cogen
    implicit val cb: Cogen[gb.A] = gb.cogen
    Gen.oneOf(
      GenT[Parser, (ga.A, gb.A)](FlatMap[Parser].product(ga.fa, gb.fa)),
      GenT[Parser, (ga.A, gb.A)](FlatMap[Parser].map2(ga.fa, gb.fa)((_, _))),
      GenT[Parser, (ga.A, gb.A)](
        FlatMap[Parser].map2Eval(ga.fa, Eval.later(gb.fa))((_, _)).value
      ),
      GenT[Parser, (ga.A, gb.A)](FlatMap[Parser].map2Eval(ga.fa, Eval.now(gb.fa))((_, _)).value)
    )
  }

  def product10(ga: GenT[Parser], gb: GenT[Parser0]): Gen[GenT[Parser]] = {
    implicit val ca: Cogen[ga.A] = ga.cogen
    implicit val cb: Cogen[gb.A] = gb.cogen
    Gen.oneOf(
      GenT[Parser, (ga.A, gb.A)](Parser0.product10(ga.fa, gb.fa)),
      GenT[Parser, ga.A](ga.fa <* gb.fa),
      GenT[Parser, gb.A](ga.fa *> gb.fa),
      GenT[Parser, (ga.A, ga.A)](Parser0.product10(ga.fa, ga.fa))
    )
  }

  def softProduct1(ga: GenT[Parser], gb: GenT[Parser0]): Gen[GenT[Parser]] = {
    implicit val ca: Cogen[ga.A] = ga.cogen
    implicit val cb: Cogen[gb.A] = gb.cogen
    Gen.oneOf(
      // left is Parser
      GenT[Parser, (ga.A, gb.A)](ga.fa.soft ~ gb.fa),
      // right is Parser
      GenT[Parser, (gb.A, ga.A)](gb.fa.with1.soft ~ ga.fa),
      // both are parser1
      GenT[Parser, (ga.A, ga.A)](ga.fa.soft ~ ga.fa)
    )
  }

  def mapped(ga: GenT[Parser0]): Gen[GenT[Parser0]] = {
    pures.flatMap { genRes =>
      implicit val ca: Cogen[ga.A] = ga.cogen
      implicit val cb: Cogen[genRes.A] = genRes.cogen
      val fnGen: Gen[ga.A => genRes.A] = Gen.function1(genRes.fa)
      fnGen.flatMap { fn =>
        Gen.oneOf(
          GenT(ga.fa.map(fn)),
          GenT(FlatMap[Parser0].map(ga.fa)(fn))
        )
      }
    }
  }

  def mapped1(ga: GenT[Parser]): Gen[GenT[Parser]] = {
    pures.flatMap { genRes =>
      implicit val ca: Cogen[ga.A] = ga.cogen
      implicit val cb: Cogen[genRes.A] = genRes.cogen
      val fnGen: Gen[ga.A => genRes.A] = Gen.function1(genRes.fa)
      fnGen.flatMap { fn =>
        Gen.oneOf(
          GenT(ga.fa.map(fn)),
          GenT(FlatMap[Parser].map(ga.fa)(fn))
        )
      }
    }
  }

  abstract class FlatMap[F[_], B] {
    type A
    val init: F[A]
    val fn: A => F[B]
  }

  def selected(ga: Gen[GenT[Parser0]]): Gen[GenT[Parser0]] =
    Gen.zip(pures, pures).flatMap { case (genRes1, genRes2) =>
      val genPR: Gen[Parser0[Either[genRes1.A, genRes2.A]]] = {
        ga.flatMap { init =>
          val mapFn: Gen[init.A => Either[genRes1.A, genRes2.A]] =
            Gen.function1(Gen.either(genRes1.fa, genRes2.fa))(init.cogen)
          mapFn.map { fn =>
            init.fa.map(fn)
          }
        }
      }

      val gfn: Gen[Parser0[genRes1.A => genRes2.A]] =
        ga.flatMap { init =>
          val mapFn: Gen[init.A => (genRes1.A => genRes2.A)] =
            Gen.function1(Gen.function1(genRes2.fa)(genRes1.cogen))(init.cogen)
          mapFn.map { fn =>
            init.fa.map(fn)
          }
        }

      Gen.zip(genPR, gfn).map { case (pab, fn) =>
        GenT(Parser0.select0(pab)(fn))(genRes2.cogen)
      }
    }

  def flatMapped(ga: Gen[GenT[Parser0]]): Gen[GenT[Parser0]] =
    Gen.zip(ga, pures).flatMap { case (parser, genRes) =>
      val genPR: Gen[Parser0[genRes.A]] = {
        ga.flatMap { init =>
          val mapFn: Gen[init.A => genRes.A] =
            Gen.function1(genRes.fa)(init.cogen)

          mapFn.map { fn =>
            init.fa.map(fn)
          }
        }
      }

      val gfn: Gen[parser.A => Parser0[genRes.A]] =
        Gen.function1(genPR)(parser.cogen)

      gfn.flatMap { fn =>
        Gen.oneOf(
          GenT(parser.fa.flatMap(fn))(genRes.cogen),
          GenT(FlatMap[Parser0].flatMap(parser.fa)(fn))(genRes.cogen)
        )
      }
    }

  // if we use a Parser0 here, we could loop forever parsing nothing
  def tailRecM(ga: Gen[GenT[Parser]]): Gen[GenT[Parser0]] =
    Gen.zip(pures, pures).flatMap { case (genRes1, genRes2) =>
      val genPR: Gen[Parser0[Either[genRes1.A, genRes2.A]]] = {
        ga.flatMap { init =>
          val mapFn: Gen[init.A => Either[genRes1.A, genRes2.A]] =
            Gen.function1(Gen.either(genRes1.fa, genRes2.fa))(init.cogen)

          mapFn.map { fn =>
            init.fa.map(fn)
          }
        }
      }

      val gfn = Gen.function1(genPR)(genRes1.cogen)

      Gen
        .zip(genRes1.fa, gfn)
        .map { case (init, fn) =>
          GenT(Monad[Parser0].tailRecM(init)(fn))(genRes2.cogen)
        }
    }

  def tailRecM1(ga: Gen[GenT[Parser]]): Gen[GenT[Parser]] =
    Gen.zip(pures, pures).flatMap { case (genRes1, genRes2) =>
      val genPR: Gen[Parser[Either[genRes1.A, genRes2.A]]] = {
        ga.flatMap { init =>
          val mapFn: Gen[init.A => Either[genRes1.A, genRes2.A]] =
            Gen.function1(Gen.either(genRes1.fa, genRes2.fa))(init.cogen)

          mapFn.map { fn =>
            init.fa.map(fn)
          }
        }
      }

      val gfn = Gen.function1(genPR)(genRes1.cogen)

      Gen
        .zip(genRes1.fa, gfn)
        .map { case (init, fn) =>
          GenT(FlatMap[Parser].tailRecM(init)(fn))(genRes2.cogen)
        }
    }

  def selected1(ga1: Gen[GenT[Parser]], ga0: Gen[GenT[Parser0]]): Gen[GenT[Parser]] =
    Gen.zip(pures, pures).flatMap { case (genRes1, genRes2) =>
      val genPR1: Gen[Parser[Either[genRes1.A, genRes2.A]]] = {
        ga1.flatMap { init =>
          val mapFn: Gen[init.A => Either[genRes1.A, genRes2.A]] =
            Gen.function1(Gen.either(genRes1.fa, genRes2.fa))(init.cogen)
          mapFn.map { fn =>
            init.fa.map(fn)
          }
        }
      }

      val gfn: Gen[Parser0[genRes1.A => genRes2.A]] =
        ga0.flatMap { init =>
          val mapFn: Gen[init.A => (genRes1.A => genRes2.A)] =
            Gen.function1(Gen.function1(genRes2.fa)(genRes1.cogen))(init.cogen)
          mapFn.map { fn =>
            init.fa.map(fn)
          }
        }

      Gen.zip(genPR1, gfn).map { case (pab, fn) =>
        GenT(Parser0.select(pab)(fn))(genRes2.cogen)
      }
    }

  def flatMapped1(ga: Gen[GenT[Parser0]], ga1: Gen[GenT[Parser]]): Gen[GenT[Parser]] =
    Gen.zip(ga, ga1, pures).flatMap { case (parser, parser1, genRes) =>
      val genPR: Gen[Parser[genRes.A]] = {
        ga1.flatMap { init =>
          val mapFn: Gen[init.A => genRes.A] =
            Gen.function1(genRes.fa)(init.cogen)

          mapFn.map { fn =>
            init.fa.map(fn)
          }
        }
      }

      val gfn: Gen[parser.A => Parser[genRes.A]] =
        Gen.function1(genPR)(parser.cogen)

      val gfn1: Gen[parser1.A => Parser[genRes.A]] =
        Gen.function1(genPR)(parser1.cogen)

      Gen.frequency(
        (
          2,
          gfn1.flatMap { fn =>
            Gen.oneOf(
              GenT(parser1.fa.flatMap(fn))(genRes.cogen), // 1 -> 0
              GenT(FlatMap[Parser].flatMap(parser1.fa)(fn))(genRes.cogen) // 1 -> 1
            )
          }
        ),
        (
          1,
          gfn.map { fn =>
            GenT(parser.fa.with1.flatMap(fn))(genRes.cogen) // 0 -> 1
          }
        )
      )
    }

  def orElse0(ga: GenT[Parser0], gb: GenT[Parser0], res: GenT[Gen]): Gen[GenT[Parser0]] = {
    val genFn1: Gen[ga.A => res.A] = Gen.function1(res.fa)(ga.cogen)
    val genFn2: Gen[gb.A => res.A] = Gen.function1(res.fa)(gb.cogen)
    implicit val cogenResA: Cogen[res.A] = res.cogen

    Gen.zip(genFn1, genFn2).flatMap { case (f1, f2) =>
      Gen.oneOf(
        GenT(ga.fa.map(f1).orElse0(gb.fa.map(f2))),
        GenT(MonoidK[Parser0].combineK(ga.fa.map(f1), gb.fa.map(f2)))
      )
    }
  }

  def orElse(ga: GenT[Parser], gb: GenT[Parser], res: GenT[Gen]): Gen[GenT[Parser]] = {
    val genFn1: Gen[ga.A => res.A] = Gen.function1(res.fa)(ga.cogen)
    val genFn2: Gen[gb.A => res.A] = Gen.function1(res.fa)(gb.cogen)
    implicit val cogenResA: Cogen[res.A] = res.cogen

    Gen.zip(genFn1, genFn2).flatMap { case (f1, f2) =>
      Gen.oneOf(
        GenT(ga.fa.map(f1).orElse(gb.fa.map(f2))),
        GenT(MonoidK[Parser].combineK(ga.fa.map(f1), gb.fa.map(f2)))
      )
    }
  }

  // Generate a random parser
  lazy val gen0: Gen[GenT[Parser0]] = {
    val rec = Gen.lzy(gen0)

    Gen.frequency(
      (
        3,
        pures
          .flatMap(_.toId)
          .map(_.transform(new FunctionK[Id, Parser0] {
            def apply[A](g: Id[A]): Parser0[A] = Parser0.pure(g)
          }))
      ),
      (5, expect0),
      (1, ignoreCase0),
      (5, charIn0),
      (1, Gen.oneOf(GenT(Parser0.start), GenT(Parser0.end), GenT(Parser0.index))),
      (1, fail),
      (1, failWith),
      (1, rec.map(void0(_))),
      (1, rec.map(string0(_))),
      (1, rec.map(backtrack0(_))),
      (1, rec.map(defer0(_))),
      (1, rec.map { gen => GenT(!gen.fa) }),
      (1, Gen.lzy(gen.map(rep0(_)))),
      (1, rec.flatMap(mapped(_))),
      (1, rec.flatMap(selected(_))),
      (1, tailRecM(Gen.lzy(gen))),
      (1, Gen.choose(0, 10).map { l => GenT(Parser0.length0(l)) }),
      (1, flatMapped(rec)),
      (1, Gen.zip(rec, rec).flatMap { case (g1, g2) => product(g1, g2) }),
      (1, Gen.zip(rec, rec).flatMap { case (g1, g2) => softProduct(g1, g2) }),
      (1, Gen.zip(rec, rec, pures).flatMap { case (g1, g2, p) => orElse0(g1, g2, p) })
    )
  }

  // Generate a random parser
  lazy val gen: Gen[GenT[Parser]] = {
    val rec = Gen.lzy(gen)

    Gen.frequency(
      (8, expect1),
      (2, ignoreCase),
      (8, charIn),
      (1, Gen.choose(Char.MinValue, Char.MaxValue).map { c => GenT(Parser0.char(c)) }),
      (2, rec.map(void(_))),
      (2, rec.map(string(_))),
      (2, rec.map(backtrack(_))),
      (1, rec.map(defer(_))),
      (1, rec.map(rep(_))),
      (1, selected1(rec, gen0)),
      (1, rec.flatMap(mapped1(_))),
      (1, flatMapped1(gen0, rec)),
      (1, tailRecM1(rec)),
      (1, Gen.choose(1, 10).map { l => GenT(Parser0.length(l)) }),
      (
        2,
        Gen.frequency(
          (1, Gen.zip(rec, rec).flatMap { case (g1, g2) => product1(g1, g2) }),
          (1, Gen.zip(rec, gen0).flatMap { case (g1, g2) => product10(g1, g2) }),
          (1, Gen.zip(rec, gen0).flatMap { case (g1, g2) => softProduct1(g1, g2) }),
          (1, Gen.zip(rec, rec, pures).flatMap { case (g1, g2, p) => orElse(g1, g2, p) })
        )
      )
    )
  }

  def genParser0[A](genA: Gen[A]): Gen[Parser0[A]] =
    for {
      genT <- gen0
      fn <- Gen.function1(genA)(genT.cogen)
    } yield genT.fa.map(fn)

  def genParser[A](genA: Gen[A]): Gen[Parser[A]] =
    for {
      genT <- gen
      fn <- Gen.function1(genA)(genT.cogen)
    } yield genT.fa.map(fn)

  implicit def arbParser0[A: Arbitrary]: Arbitrary[Parser0[A]] =
    Arbitrary(genParser0(Arbitrary.arbitrary[A]))

  implicit def arbParser[A: Arbitrary]: Arbitrary[Parser[A]] =
    Arbitrary(genParser(Arbitrary.arbitrary[A]))
}

class ParserTest extends munit.ScalaCheckSuite {

  import ParserGen.{arbParser0, arbParser}

  val tests: Int = if (BitSetUtil.isScalaJs) 50 else 2000

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(tests)
      .withMaxDiscardRatio(10)

  def parseTest[A: Eq](p: Parser0[A], str: String, a: A) =
    p.parse(str) match {
      case Right((_, res)) =>
        assert(Eq[A].eqv(a, res), s"expected: $a got $res")
      case Left(errs) =>
        assert(false, errs.toString)
    }

  def parseFail[A: Eq](p: Parser0[A], str: String) =
    p.parse(str) match {
      case Right(res) =>
        assert(false, s"expected to not parse, but found: $res")
      case Left(_) =>
        assert(true)
    }

  test("pure works") {
    parseTest(Parser0.pure(42), "anything", 42)
  }

  val fooP = Parser0.string("foo")
  val barP = Parser0.string("bar")
  val fooCIP = Parser0.ignoreCase("foo")
  val cCIP = Parser0.ignoreCase("a")
  val cCIP1 = Parser0.ignoreCaseChar('a')
  val abcCI = Parser0.ignoreCaseCharIn('a', 'b', 'c')

  test("string tests") {
    parseTest(fooP, "foobar", ())
    parseFail(fooP, "FOO")
    parseTest(fooCIP, "FoO", ())
    parseTest(cCIP, "A", ())
    parseTest(cCIP, "a", ())
    parseTest(cCIP1, "A", ())
    parseTest(cCIP1, "a", ())
    parseFail(fooP, "bar")

    parseTest(abcCI, "a", 'a')
    parseTest(abcCI, "A", 'A')
    parseTest(abcCI, "b", 'b')
    parseTest(abcCI, "B", 'B')
    parseTest(abcCI, "c", 'c')
    parseTest(abcCI, "C", 'C')
    parseFail(abcCI, "D")

    parseTest(Parser0.oneOf(fooP :: barP :: Nil), "bar", ())
    parseTest(Parser0.oneOf(fooP :: barP :: Nil), "foo", ())
  }

  test("product tests") {
    parseTest(Parser0.product01(fooP, barP), "foobar", ((), ()))
    parseTest(Parser0.product10(fooP, barP), "foobar", ((), ()))
    parseTest(Parser0.product(fooP, barP), "foobar", ((), ()))
  }

  property("Parser0 on success replaces parsed value") {
    forAll(ParserGen.gen0, Arbitrary.arbitrary[String]) { (genP, str) =>
      val res0 = genP.fa.as("something").parse(str)
      res0 match {
        case Left(_) => ()
        case Right((_, v)) => assertEquals(v, "something")
      }
    }
  }

  property("Parser0.start and end work") {
    forAll { (s: String) =>
      if (s.isEmpty) {
        intercept[IllegalArgumentException] {
          Parser0.string(s)
        }
      } else {
        val pa = Parser0.string(s)
        assertEquals((Parser0.start ~ pa ~ Parser0.end).void.parse(s), Right(("", ())))
        assert((pa ~ Parser0.start).parse(s).isLeft)
        assert((Parser0.end ~ pa).parse(s).isLeft)
        assertEquals(
          (Parser0.index ~ pa ~ Parser0.index).map { case ((s, _), e) => e - s }.parse(s),
          Right(("", s.length))
        )
      }

      true
    }
  }

  property("Parser0.length0 succeeds when the string is long enough") {
    forAll { (s: String, len: Int) =>
      if (len < 1) {
        intercept[IllegalArgumentException] {
          Parser0.length(len)
        }
        assertEquals(Parser0.length0(len).parse(s), Right((s, "")))
      } else {
        val pa = Parser0.length0(len)
        val pa1 = Parser0.length(len)

        val res = pa.parse(s)
        val res1 = pa1.parse(s)

        assertEquals(res, res1)

        res match {
          case Right((rest, first)) =>
            if (s.length >= len) {
              assertEquals(s.take(len), first)
              assertEquals(s.drop(len), rest)
            } else fail(s"expected to not parse: $rest, $first")
          case Left(Parser0.Error(0, NonEmptyList(Parser0.Expectation.Length(off, l, a), Nil))) =>
            assertEquals(off, 0)
            assertEquals(l, len)
            assertEquals(a, s.length)
          case Left(other) =>
            fail(s"unexpected error: $other")
        }
      }

      true
    }
  }

  property("voided only changes the result") {
    forAll(ParserGen.gen0, Arbitrary.arbitrary[String]) { (genP, str) =>
      val r1 = genP.fa.parse(str)
      val r2 = genP.fa.void.parse(str)
      val r3 = FlatMap[Parser0].void(genP.fa).parse(str)
      val r4 = genP.fa.as(()).parse(str)

      assertEquals(r2, r1.map { case (off, _) => (off, ()) })
      assertEquals(r2, r3)
      assertEquals(r2, r4)
    }
  }

  property("voided only changes the result Parser") {
    forAll(ParserGen.gen, Arbitrary.arbitrary[String]) { (genP, str) =>
      val r1 = genP.fa.parse(str)
      val r2 = genP.fa.void.parse(str)
      val r3 = FlatMap[Parser].void(genP.fa).parse(str)
      val r4 = genP.fa.as(()).parse(str)
      val r5 = ((genP.fa.void: Parser0[Unit]) <* Monad[Parser0].unit).parse(str)

      assertEquals(r2, r1.map { case (off, _) => (off, ()) })
      assertEquals(r2, r3)
      assertEquals(r2, r4)
      assertEquals(r2, r5)
    }
  }

  property("expected in errors gives valid offsets") {
    forAll(ParserGen.gen0, Arbitrary.arbitrary[String]) { (genP, str) =>
      genP.fa.parse(str) match {
        case Left(err) =>
          err.offsets.forall { off =>
            (0 <= off) && (off <= str.length)
          }
        case Right(_) => true
      }

    }
  }

  property("oneOf0 nesting doesn't change results") {
    forAll(Gen.listOf(ParserGen.gen0), Gen.listOf(ParserGen.gen0), Arbitrary.arbitrary[String]) {
      (genP1, genP2, str) =>
        val oneOf = Parser0.oneOf0((genP1 ::: genP2).map(_.fa))
        val oneOf2 = Parser0.oneOf0(genP1.map(_.fa)).orElse0(Parser0.oneOf0(genP2.map(_.fa)))

        assertEquals(oneOf.parse(str), oneOf2.parse(str))
    }
  }

  property("oneOf nesting doesn't change results") {
    forAll(Gen.listOf(ParserGen.gen), Gen.listOf(ParserGen.gen), Arbitrary.arbitrary[String]) {
      (genP1, genP2, str) =>
        val oneOf = Parser0.oneOf((genP1 ::: genP2).map(_.fa))
        val oneOf2 = Parser0
          .oneOf(genP1.map(_.fa))
          .orElse(
            Parser0.oneOf(genP2.map(_.fa))
          )

        assertEquals(oneOf.parse(str), oneOf2.parse(str))
    }
  }

  def orElse[A](p1: Parser0[A], p2: Parser0[A], str: String): Either[Parser0.Error, (String, A)] = {
    if (p1 == Parser0.Fail) p2.parse(str)
    else if (p2 == Parser0.Fail) p1.parse(str)
    else
      p1.parse(str) match {
        case left @ Left(err) =>
          if (err.failedAtOffset == 0) {
            p2.parse(str)
              .leftMap { err1 =>
                if (err1.failedAtOffset == 0) {
                  val errs = err.expected ::: err1.expected
                  Parser0.Error(err1.failedAtOffset, Parser0.Expectation.unify(errs))
                } else err1
              }
          } else left
        case right => right
      }
  }

  property("oneOf0 composes as expected") {
    forAll(ParserGen.gen0, ParserGen.gen0, Arbitrary.arbitrary[String]) { (genP1, genP2, str) =>
      assertEquals(genP1.fa.orElse0(genP2.fa).parse(str), orElse(genP1.fa, genP2.fa, str))
    }
  }

  property("oneOf composes as expected") {
    forAll(ParserGen.gen, ParserGen.gen, Arbitrary.arbitrary[String]) { (genP1, genP2, str) =>
      assertEquals(genP1.fa.orElse(genP2.fa).parse(str), orElse(genP1.fa, genP2.fa, str))
    }
  }

  property("oneOf0 same as foldLeft(fail)(_.orElse0(_))") {
    forAll(Gen.listOf(ParserGen.gen0), Arbitrary.arbitrary[String]) { (genP1, str) =>
      val oneOfImpl = genP1.foldLeft(Parser0.fail: Parser0[Any]) { (leftp, p) => leftp.orElse0(p.fa) }

      assertEquals(oneOfImpl.parse(str), Parser0.oneOf0(genP1.map(_.fa)).parse(str))
    }
  }

  property("oneOf same as foldLeft(fail)(_.orElse(_))") {
    forAll(Gen.listOf(ParserGen.gen), Arbitrary.arbitrary[String]) { (genP1, str) =>
      val oneOfImpl = genP1.foldLeft(Parser0.fail[Any]) { (leftp, p) => leftp.orElse(p.fa) }

      assertEquals(oneOfImpl.parse(str), Parser0.oneOf(genP1.map(_.fa)).parse(str))
    }
  }

  property("string can be recovered with index") {
    forAll(ParserGen.gen0, Arbitrary.arbitrary[String]) { (genP, str) =>
      val r1 = genP.fa.string.parse(str)
      val r2 = (genP.fa ~ Parser0.index).map { case (_, end) => str.substring(0, end) }.parse(str)

      assertEquals(r1.toOption, r2.toOption)
    }
  }

  property("backtrack orElse pure always succeeds") {
    forAll(ParserGen.gen0, Arbitrary.arbitrary[String]) { (genP, str) =>
      val p1 = genP.fa.backtrack.orElse0(Parser0.pure(())): Parser0[Any]

      assert(p1.parse(str).isRight)
    }
  }

  property("backtrack.? pure always succeeds") {
    forAll(ParserGen.gen0, Arbitrary.arbitrary[String]) { (genP, str) =>
      val p1 = genP.fa.backtrack.?

      assert(p1.parse(str).isRight)
    }
  }

  property("a.backtrack either succeeds or fails at 0") {
    forAll(ParserGen.gen0, Arbitrary.arbitrary[String]) { (a, str) =>
      a.fa.backtrack.parse(str) match {
        case Right(_) => ()
        case Left(err) => assertEquals(err.failedAtOffset, 0)
      }
    }
  }

  property("a ~ b composes as expected") {
    forAll(ParserGen.gen0, ParserGen.gen0, Arbitrary.arbitrary[String]) { (p1, p2, str) =>
      val composed = p1.fa ~ p2.fa
      val cres = composed.parse(str)

      val composed1 = Monad[Parser0].product(p1.fa, p2.fa)
      composed1.parse(str)

      val sequence =
        for {
          pair1 <- p1.fa.parse(str)
          (s1, a1) = pair1
          off = if (s1 == "") str.length else str.indexOf(s1)
          // make the offsets the same
          sfix = " " * off + s1
          p3 = (Parser0.length0(off) ~ p2.fa).map(_._2)
          pair2 <- p3.parse(sfix)
          (s2, a2) = pair2
        } yield (s2, (a1, a2))

      assertEquals(cres, sequence)
    }
  }

  property("a ~ b composes as expected parser1") {
    forAll(ParserGen.gen, ParserGen.gen, Arbitrary.arbitrary[String]) { (p1, p2, str) =>
      val composed = p1.fa ~ p2.fa
      val cres = composed.parse(str)

      val composed1 = FlatMap[Parser0].product(p1.fa, p2.fa)
      val cres1 = composed1.parse(str)
      assertEquals(cres, cres1)

      val sequence =
        for {
          pair1 <- p1.fa.parse(str)
          (s1, a1) = pair1
          off = if (s1 == "") str.length else str.indexOf(s1)
          // make the offsets the same
          sfix = " " * off + s1
          p3 = Parser0.length0(off) *> p2.fa
          pair2 <- p3.parse(sfix)
          (s2, a2) = pair2
        } yield (s2, (a1, a2))

      assertEquals(cres, sequence)
    }
  }

  property("a.with1 ~ b composes as expected") {
    forAll(ParserGen.gen0, ParserGen.gen, Arbitrary.arbitrary[String]) { (p1, p2, str) =>
      val composed = p1.fa.with1 ~ p2.fa
      val cres = composed.parse(str)

      val sequence =
        for {
          pair1 <- p1.fa.parse(str)
          (s1, a1) = pair1
          off = if (s1 == "") str.length else str.indexOf(s1)
          // make the offsets the same
          sfix = " " * off + s1
          p3 = Parser0.length0(off) *> p2.fa
          pair2 <- p3.parse(sfix)
          (s2, a2) = pair2
        } yield (s2, (a1, a2))

      assertEquals(cres, sequence)
    }
  }

  property("a.soft ~ b composes as expected") {
    forAll(ParserGen.gen0, ParserGen.gen0, Arbitrary.arbitrary[String]) { (p1, p2, str) =>
      val composed = p1.fa.soft ~ p2.fa
      val cres = composed.parse(str)

      val sequence =
        for {
          pair1 <- p1.fa.parse(str)
          (s1, a1) = pair1
          off = if (s1 == "") str.length else str.indexOf(s1)
          // make the offsets the same
          sfix = " " * off + s1
          p3 = (Parser0.length0(off) ~ p2.fa).map(_._2)
          pair2 <- (p3.parse(sfix).leftMap {
            case Parser0.Error(fidx, errs) if (fidx == off) => Parser0.Error(0, errs)
            case notEps2 => notEps2
          })
          (s2, a2) = pair2
        } yield (s2, (a1, a2))

      assertEquals(cres, sequence)
    }
  }

  property("a1.soft ~ b composes as expected Parser") {
    forAll(ParserGen.gen, ParserGen.gen0, Arbitrary.arbitrary[String]) { (p1, p2, str) =>
      val composed = p1.fa.soft ~ p2.fa
      val cres = composed.parse(str)

      val sequence =
        for {
          pair1 <- p1.fa.parse(str)
          (s1, a1) = pair1
          off = if (s1 == "") str.length else str.indexOf(s1)
          // make the offsets the same
          sfix = " " * off + s1
          p3 = (Parser0.length0(off) ~ p2.fa).map(_._2)
          pair2 <- (p3.parse(sfix).leftMap {
            case Parser0.Error(fidx, errs) if (fidx == off) => Parser0.Error(0, errs)
            case notEps2 => notEps2
          })
          (s2, a2) = pair2
        } yield (s2, (a1, a2))

      assertEquals(cres, sequence)
    }
  }

  property("a.with1.soft ~ b1 composes as expected") {
    forAll(ParserGen.gen0, ParserGen.gen, Arbitrary.arbitrary[String]) { (p1, p2, str) =>
      val composed = p1.fa.with1.soft ~ p2.fa
      val cres = composed.parse(str)

      val sequence =
        for {
          pair1 <- p1.fa.parse(str)
          (s1, a1) = pair1
          off = if (s1 == "") str.length else str.indexOf(s1)
          // make the offsets the same
          sfix = " " * off + s1
          p3 = (Parser0.length0(off) ~ p2.fa).map(_._2)
          pair2 <- (p3.parse(sfix).leftMap {
            case Parser0.Error(fidx, errs) if (fidx == off) => Parser0.Error(0, errs)
            case notEps2 => notEps2
          })
          (s2, a2) = pair2
        } yield (s2, (a1, a2))

      assertEquals(cres, sequence)
    }
  }

  test("range messages seem to work") {
    val pa = Parser0.charIn('0' to '9')
    assertEquals(pa.parse("z").toString, "Left(Error(0,NonEmptyList(InRange(0,0,9))))")
  }

  test("partial parse fails in rep0") {
    val partial = Parser0.length(1) ~ Parser0.fail
    // we can't return empty list here
    assert(partial.rep0.parse("foo").isLeft)

    val p2 = Parser0.string("f").orElse((Parser0.string("boo") ~ Parser0.string("p")).void)
    assert(p2.rep.parse("fboop").isRight)
    assert(p2.rep(2).parse("fboop").isRight)
    assert(p2.rep(3).parse("fboop").isLeft)
    assert(p2.rep.parse("fboof").isLeft)
  }

  test("defer does not run eagerly") {
    var cnt = 0
    val res = Defer[Parser0].defer {
      cnt += 1
      Parser0.string("foo")
    }
    assert(cnt == 0)
    assert(res.parse("foo") == Right(("", ())))
    assert(cnt == 1)
    assert(res.parse("foo") == Right(("", ())))
    assert(cnt == 1)
  }

  test("defer does not run eagerly") {
    var cnt = 0
    val res = Defer[Parser].defer {
      cnt += 1
      Parser0.string("foo")
    }
    assert(cnt == 0)
    assert(res.parse("foo") == Right(("", ())))
    assert(cnt == 1)
    assert(res.parse("foo") == Right(("", ())))
    assert(cnt == 1)
  }

  property("charIn matches charWhere") {
    forAll { (cs: List[Char], str: String) =>
      val cset = cs.toSet
      val p1 = Parser0.charIn(cs)
      val p2 = Parser0.charWhere(cset)

      assertEquals(p1.parse(str), p2.parse(str))
    }
  }

  property("charIn matches charIn varargs") {
    forAll { (c0: Char, cs0: List[Char], str: String) =>
      val cs = c0 :: cs0
      val p1 = Parser0.charIn(cs)
      val p2 = Parser0.charIn(c0, cs0: _*)

      assertEquals(p1.parse(str), p2.parse(str))
    }
  }

  property("Parser0.end gives the right error") {
    forAll { (str: String) =>
      Parser0.end.parse(str) match {
        case Right((rest, _)) =>
          assertEquals(str, "")
          assertEquals(rest, "")
        case Left(Parser0.Error(0, NonEmptyList(Parser0.Expectation.EndOfString(off, len), Nil))) =>
          assertEquals(off, 0)
          assertEquals(len, str.length)
        case other =>
          fail(s"unexpected failure: $other")
      }
    }
  }

  property("rep0 can be reimplemented with oneOf0 and defer") {
    forAll(ParserGen.gen, Arbitrary.arbitrary[String]) { (genP, str) =>
      def rep0[A](pa: Parser[A]): Parser0[List[A]] =
        Defer[Parser0].fix[List[A]] { tail =>
          (pa ~ tail)
            .map { case (h, t) => h :: t }
            .orElse0(Parser0.pure(Nil))
        }

      val lst1 = rep0(genP.fa)
      val lst2 = genP.fa.rep0

      assertEquals(lst1.parse(str), lst2.parse(str))
    }
  }

  property("rep0 is consistent with rep") {
    forAll(ParserGen.gen, Gen.choose(0, Int.MaxValue), Arbitrary.arbitrary[String]) {
      (genP, min0, str) =>
        val min = min0 & Int.MaxValue
        val repA = genP.fa.rep0(min)
        val repB = genP.fa
          .rep(min)
          .map(_.toList)
          .orElse0(
            if (min == 0) Parser0.pure(Nil)
            else Parser0.fail
          )

        assertEquals(repA.parse(str), repB.parse(str))
    }
  }

  property("repSep with unit sep is the same as rep0") {
    forAll(ParserGen.gen, Gen.choose(0, Int.MaxValue), Arbitrary.arbitrary[String]) {
      (genP, min0, str) =>
        val min = min0 & Int.MaxValue
        val p1a = Parser0.repSep(genP.fa, min = min, sep = Parser0.unit)
        val p1b = genP.fa.rep0(min = min)

        assertEquals(p1a.parse(str), p1b.parse(str))

        val min1 = if (min < 1) 1 else min
        val p2a = Parser0.rep1Sep(genP.fa, min = min1, sep = Parser0.unit)
        val p2b = genP.fa.rep(min = min1)

        assertEquals(p2a.parse(str), p2b.parse(str))
    }
  }

  property("rep1Sep with sep = fail is the same as parsing 1") {
    forAll(ParserGen.gen, Arbitrary.arbitrary[String]) { (genP, str) =>
      assertEquals(
        genP.fa.parse(str),
        Parser0.rep1Sep(genP.fa, 1, Parser0.fail).parse(str).map { case (rest, nel) =>
          (rest, nel.head)
        }
      )
    }
  }

  property("charsWhile/charWhile consistency") {
    forAll(
      Gen.choose(0, 100).flatMap(Gen.listOfN(_, Gen.choose(Char.MinValue, Char.MaxValue))),
      Arbitrary.arbitrary[String]
    ) { (chars, str) =>
      val pred = chars.toSet
      val p1a = Parser0.charsWhile(pred)
      val p1b = Parser0.charWhere(pred).rep0.string
      assertEquals(p1a.parse(str), p1b.parse(str))

      val p2a = Parser0.charsWhile1(pred)
      val p2b = Parser0.charWhere(pred).rep.string
      assertEquals(p2a.parse(str), p2b.parse(str))
    }
  }

  property("MonoidK[Parser0].empty never succeeds") {
    forAll { (str: String) =>
      assert(MonoidK[Parser0].empty.parse(str).isLeft)
      assert(MonoidK[Parser].empty.parse(str).isLeft)
    }
  }

  property("Monad.pure is an identity function") {
    forAll { (i: Int, str: String) =>
      assertEquals(Monad[Parser0].pure(i).parse(str), Right((str, i)))
    }
  }

  property("p orElse p == p") {
    forAll(ParserGen.gen, Arbitrary.arbitrary[String]) { (genP, str) =>
      val res0 = genP.fa.parse(str)
      val res1 = genP.fa.orElse0(genP.fa).parse(str)
      assertEquals(res1, res0)
    }
  }

  property("p orElse p == p") {
    forAll(ParserGen.gen, Arbitrary.arbitrary[String]) { (genP, str) =>
      val res0 = genP.fa.parse(str)
      val res1 = genP.fa.orElse(genP.fa).parse(str)
      assertEquals(res1, res0)
    }
  }

  property("Parser fails or consumes 1 or more") {
    forAll(ParserGen.gen, Arbitrary.arbitrary[String]) { (genP, str) =>
      val res0 = genP.fa.parse(str)
      res0 match {
        case Left(_) => assert(true)
        case Right((s, _)) => assert(str != s)
      }
    }
  }

  property("p1.backtrack.orElse0(p2) succeeds if either p1 or p2 do") {
    forAll(ParserGen.gen0, ParserGen.gen0, Arbitrary.arbitrary[String]) { (p1, p2, str) =>
      val ores = p1.fa.backtrack.orElse0(p2.fa).parse(str)
      val r1 = p1.fa.parse(str)
      val r = if (r1.isLeft) p2.fa.parse(str) else r1
      (ores, r) match {
        case (Left(_), l) => assert(l.isLeft)
        case (ra, rb) => assertEquals(ra, rb)
      }
    }
  }

  property("p1.backtrack.orElse(p2) succeeds if either p1 or p2 do") {
    forAll(ParserGen.gen, ParserGen.gen, Arbitrary.arbitrary[String]) { (p1, p2, str) =>
      val ores = p1.fa.backtrack.orElse(p2.fa).parse(str)
      val r1 = p1.fa.parse(str)
      val r = if (r1.isLeft) p2.fa.parse(str) else r1
      (ores, r) match {
        case (Left(_), l) => assert(l.isLeft)
        case (ra, rb) => assertEquals(ra, rb)
      }
    }
  }

  test("charWhere(_ => true) == anyChar") {
    assertEquals(Parser0.charWhere(_ => true), Parser0.anyChar)
  }

  property("with1 *> and with1 <* work as expected") {
    forAll(ParserGen.gen0, ParserGen.gen, Arbitrary.arbitrary[String]) { (p1, p2, str) =>
      val rp1 = p1.fa.with1 *> p2.fa
      val rp2 = (p1.fa.with1 ~ p2.fa).map(_._2)
      assertEquals(rp1.parse(str), rp2.parse(str))

      val rp3 = p1.fa.with1 <* p2.fa
      val rp4 = (p1.fa.with1 ~ p2.fa).map(_._1)
      assertEquals(rp3.parse(str), rp4.parse(str))
    }
  }

  property("a1 *> b and a1 <* b") {
    forAll(ParserGen.gen, ParserGen.gen0, Arbitrary.arbitrary[String]) { (p1, p2, str) =>
      assertEquals(
        (p1.fa *> p2.fa).parse(str),
        Parser0.product10(p1.fa.void, p2.fa).map(_._2).parse(str)
      )
      assertEquals(
        (p1.fa <* p2.fa).parse(str),
        Parser0.product10(p1.fa, p2.fa.void).map(_._1).parse(str)
      )
    }
  }

  property("parse between open and close") {
    forAll(ParserGen.gen0, ParserGen.gen0, ParserGen.gen0, Arbitrary.arbitrary[String]) {
      (genP1, genP, genQ, str) =>
        val pa = genP1.fa.between(genP.fa, genQ.fa)
        val pb = genP.fa *> genP1.fa <* genQ.fa

        assertEquals(pa.parse(str), pb.parse(str))
    }
  }

  property("surroundedBy consistent with between") {
    forAll(ParserGen.gen0, ParserGen.gen0, Arbitrary.arbitrary[String]) { (genP1, genP, str) =>
      val pa = genP1.fa.between(genP.fa, genP.fa)
      val pb = genP1.fa.surroundedBy(genP.fa)

      assertEquals(pa.parse(str), pb.parse(str))
    }
  }

  property("parse between open and close with Parser this") {
    forAll(ParserGen.gen, ParserGen.gen0, ParserGen.gen0, Arbitrary.arbitrary[String]) {
      (genP1, genP, genQ, str) =>
        val pa = genP1.fa.between(genP.fa, genQ.fa)
        val pb = genP.fa *> genP1.fa <* genQ.fa

        assertEquals(pa.parse(str), pb.parse(str))
    }
  }

  property("surroundedBy consistent with between with Parser this") {
    forAll(ParserGen.gen, ParserGen.gen0, Arbitrary.arbitrary[String]) { (genP1, genP, str) =>
      val pa = genP1.fa.between(genP.fa, genP.fa)
      val pb = genP1.fa.surroundedBy(genP.fa)

      assertEquals(pa.parse(str), pb.parse(str))
    }
  }

  property("parse between open and close with Parser args") {
    forAll(ParserGen.gen0, ParserGen.gen, ParserGen.gen, Arbitrary.arbitrary[String]) {
      (genP1, genP, genQ, str) =>
        val pa = genP1.fa.with1.between(genP.fa, genQ.fa)
        val pb = genP.fa *> genP1.fa <* genQ.fa

        assertEquals(pa.parse(str), pb.parse(str))
    }
  }

  property("surroundedBy consistent with between with Parser this") {
    forAll(ParserGen.gen, ParserGen.gen0, Arbitrary.arbitrary[String]) { (genP1, genP, str) =>
      val pa = genP1.fa.between(genP.fa, genP.fa)
      val pb = genP1.fa.surroundedBy(genP.fa)

      assertEquals(pa.parse(str), pb.parse(str))
    }
  }

  property("exactly one of x or !x parse") {
    forAll(ParserGen.gen0, Arbitrary.arbitrary[String]) { (p1, str) =>
      val notx = !p1.fa

      val xor = p1.fa.parse(str).isRight ^ notx.parse(str).isRight
      assert(xor)
    }
  }

  property("if x ~ y matches then x ~ y.peek match") {
    forAll(ParserGen.gen0, ParserGen.gen0, Arbitrary.arbitrary[String]) { (x, y, str) =>
      val m1 = (x.fa ~ y.fa).parse(str)
      val m2 = ((x.fa ~ y.fa.peek).map(_._1)).parse(str)

      assertEquals(m1.isRight, m2.isRight)
      if (m1.isRight) {
        assert(x.fa.parse(str) == m2)
      }
    }
  }

  property("if x matches then x.peek matches but returns the whole string and unit") {
    forAll(ParserGen.gen0, Arbitrary.arbitrary[String]) { (x, str) =>
      if (x.fa.parse(str).isRight) {
        assertEquals(x.fa.peek.parse(str), Right((str, ())))
      }
    }
  }

  property("(a.soft ~ b) == a ~ b in success of expected (not partials)") {
    forAll(ParserGen.gen0, ParserGen.gen0, Arbitrary.arbitrary[String]) { (a, b, str) =>
      val left = a.fa.soft ~ b.fa
      val right = a.fa ~ b.fa
      val leftRes = left.parse(str).leftMap(_.expected)
      val rightRes = right.parse(str).leftMap(_.expected)
      assertEquals(leftRes, rightRes)
    }
  }

  property("(a.soft ~ b) == softProduct(a, b)") {
    forAll(ParserGen.gen0, ParserGen.gen0, Arbitrary.arbitrary[String]) { (a, b, str) =>
      val left = a.fa.soft ~ b.fa
      val right = Parser0.softProduct(a.fa, b.fa)
      assertEquals(left.parse(str), right.parse(str))
      assertEquals(
        (a.fa.soft *> b.fa).parse(str),
        Parser0.softProduct(a.fa.void, b.fa).map(_._2).parse(str)
      )
      assertEquals(
        (a.fa.soft <* b.fa).parse(str),
        Parser0.softProduct(a.fa, b.fa.void).map(_._1).parse(str)
      )
    }
  }

  property("(a1.soft ~ b) == softProduct10(a, b)") {
    forAll(ParserGen.gen, ParserGen.gen0, Arbitrary.arbitrary[String]) { (a, b, str) =>
      val left1 = a.fa.soft ~ b.fa
      val right1 = Parser0.softProduct10(a.fa, b.fa)
      assertEquals(left1.parse(str), right1.parse(str))

      val left2 = b.fa.soft ~ a.fa
      val right2 = Parser0.softProduct01(b.fa, a.fa)
      assertEquals(left2.parse(str), right2.parse(str))

      assertEquals(
        (a.fa.soft *> b.fa).parse(str),
        Parser0.softProduct10(a.fa.void, b.fa).map(_._2).parse(str)
      )
      assertEquals(
        (b.fa.with1.soft <* a.fa).parse(str),
        Parser0.softProduct01(b.fa, a.fa.void).map(_._1).parse(str)
      )
      assertEquals(
        (b.fa.with1.soft *> a.fa).parse(str),
        Parser0.softProduct01(b.fa.void, a.fa).map(_._2).parse(str)
      )
    }
  }

  property("Parser0.until is like a search") {
    forAll(ParserGen.gen0, Arbitrary.arbitrary[String]) { (a, str) =>
      val p = Parser0.until(a.fa) *> a.fa
      def loopMatch(cnt: Int): Option[(String, a.A)] =
        (Parser0.length0(cnt) *> a.fa).parse(str) match {
          case Right(res) => Some(res)
          case Left(_) if cnt > str.length => None
          case _ => loopMatch(cnt + 1)
        }

      assertEquals(p.parse(str).toOption, loopMatch(0))
    }
  }

  property("parseAll law") {
    forAll(ParserGen.gen0, Arbitrary.arbitrary[String]) { (a, str) =>
      val pall = (a.fa <* Parser0.end).parse(str).map(_._2)

      assertEquals(a.fa.parseAll(str), pall)
    }
  }

  property("BitSetUtil union works") {
    forAll { (cs: List[List[Char]]) =>
      val arys = cs.filter(_.nonEmpty).map(_.toArray.sorted)
      val bs = arys.map { ary => (ary(0).toInt, BitSetUtil.bitSetFor(ary)) }
      val sortedFlat = BitSetUtil.union(bs)
      assertEquals(sortedFlat.toSet, cs.flatten.toSet)
    }
  }

  /*
   * it would be nice if parsers were purely distributive, but they are not.
   * While cats Alternative laws do require some weak distributivity, Haskell
   * does not:
   *
   * https://en.wikibooks.org/wiki/Haskell/Alternative_and_MonadPlus#Other_suggested_laws
   *
   * see a related cats discussion here:
   * https://github.com/typelevel/cats/pull/1345
   *
   * Instead, we have some weakened versions of distributive laws
   */
  property("b.orElse0(c) ~ a == (b ~ a).orElse0((!b) *> (c ~ a))") {
    forAll(ParserGen.gen0, ParserGen.gen0, ParserGen.gen0, Arbitrary.arbitrary[String]) {
      (a, b, c, str) =>
        val pa = a.fa
        val pb = b.fa
        val pc = c.fa

        val left = pb.orElse0(pc) ~ pa
        val right = (pb ~ pa).orElse0((!pb) *> (pc ~ pa))

        val leftRes = left.parse(str).toOption
        val rightRes = right.parse(str).toOption
        assertEquals(leftRes, rightRes)
    }
  }

  property("b.orElse(c) ~ a == (b ~ a).orElse((!b) *> (c ~ a))") {
    forAll(ParserGen.gen0, ParserGen.gen, ParserGen.gen, Arbitrary.arbitrary[String]) {
      (a, b, c, str) =>
        val pa = a.fa
        val pb = b.fa
        val pc = c.fa

        val left = pb.orElse(pc) ~ pa
        val right = (pb ~ pa).orElse((!pb).with1 *> (pc ~ pa))

        val leftRes = left.parseAll(str).toOption
        val rightRes = right.parseAll(str).toOption
        if (leftRes.isDefined && rightRes.isDefined) {
          assertEquals(leftRes, rightRes)
        } else ()
    }
  }

  property("a ~ b.orElse0(c) == (a.soft ~ b).orElse0(a ~ c)") {
    forAll(ParserGen.gen0, ParserGen.gen0, ParserGen.gen0, Arbitrary.arbitrary[String]) {
      (a, b, c, str) =>
        val pa = a.fa
        val pb = b.fa
        val pc = c.fa

        val left = pa ~ pb.orElse0(pc)
        val right = (pa.soft ~ pb).orElse0(pa ~ pc)

        val leftRes = left.parse(str).toOption
        val rightRes = right.parse(str).toOption
        assertEquals(leftRes, rightRes)
    }
  }

  property("a ~ b.orElse(c) == (a.soft ~ b).orElse(a ~ c)") {
    forAll(ParserGen.gen0, ParserGen.gen, ParserGen.gen, Arbitrary.arbitrary[String]) {
      (a, b, c, str) =>
        val pa = a.fa
        val pb = b.fa
        val pc = c.fa

        val left = pa ~ pb.orElse(pc)
        val right = (pa.soft.with1 ~ pb).orElse0(pa.with1 ~ pc)

        val leftRes = left.parse(str).toOption
        val rightRes = right.parse(str).toOption
        assertEquals(leftRes, rightRes)
    }
  }

  property("a.backtrack.orElse0(b) parses iff b.backtrack.orElse0(a)") {
    forAll(ParserGen.gen0, ParserGen.gen0, Arbitrary.arbitrary[String]) { (a, b, str) =>
      val pa = a.fa
      val pb = b.fa

      val left = pa.backtrack.orElse0(pb)
      val right = pb.backtrack.orElse0(pa)

      val leftRes = left.parse(str)
      val rightRes = right.parse(str)
      assertEquals(leftRes.toOption.isDefined, rightRes.toOption.isDefined)
    }
  }

  property("a.backtrack.orElse(b) parses iff b.backtrack.orElse(a)") {
    forAll(ParserGen.gen, ParserGen.gen, Arbitrary.arbitrary[String]) { (a, b, str) =>
      val pa = a.fa
      val pb = b.fa

      val left = pa.backtrack.orElse(pb)
      val right = pb.backtrack.orElse(pa)

      val leftRes = left.parse(str)
      val rightRes = right.parse(str)
      assertEquals(leftRes.toOption.isDefined, rightRes.toOption.isDefined)
    }
  }

  property("failWith returns the given error message") {
    forAll { (str: String, mes: String) =>
      assertEquals(
        Parser0.failWith(mes).parse(str),
        Left(Parser0.Error(0, NonEmptyList.of(Parser0.Expectation.FailWith(0, mes))))
      )
    }
  }

  property("failWith.? returns None") {
    forAll { (str: String, mes: String) =>
      assertEquals(Parser0.failWith(mes).?.parse(str), Right((str, None)))
    }
  }

  property("a.repAs0[Vector[A]] matches a.rep0.map(_.toVector)") {
    forAll(ParserGen.gen, Arbitrary.arbitrary[String]) { (a, str) =>
      val pa: Parser[a.A] = a.fa

      val left = pa.repAs0[Vector[a.A]]
      val right = pa.rep0.map(_.toVector)

      val leftRes = left.parse(str)
      val rightRes = right.parse(str)
      assertEquals(leftRes, rightRes)
    }
  }

  property("a.repAs[Vector[A]] matches a.rep.map(_.toList.toVector)") {
    forAll(ParserGen.gen, Arbitrary.arbitrary[String]) { (a, str) =>
      val pa: Parser[a.A] = a.fa

      val left = pa.repAs0[Vector[a.A]]
      val right = pa.rep0.map(_.toList.toVector)

      val leftRes = left.parse(str)
      val rightRes = right.parse(str)
      assertEquals(leftRes, rightRes)
    }
  }

  property("a.string.repAs0[String] matches a.string.rep0.map(_.mkString)") {
    forAll(ParserGen.gen, Arbitrary.arbitrary[String]) { (a, str) =>
      val pa: Parser[String] = a.fa.string

      val left = pa.repAs0[String]
      val right = pa.rep0.map(_.mkString)

      val leftRes = left.parse(str)
      val rightRes = right.parse(str)
      assertEquals(leftRes, rightRes)
    }
  }

  property("a.repAs0[Unit] matches a.rep0.void") {
    forAll(ParserGen.gen, Arbitrary.arbitrary[String]) { (a, str) =>
      val pa: Parser[a.A] = a.fa

      val left = pa.repAs0[Unit](Accumulator0.unitAccumulator0)
      val right = pa.rep0.void

      val leftRes = left.parse(str)
      val rightRes = right.parse(str)
      assertEquals(leftRes, rightRes)
    }
  }

  property("a.peek == a.peek.peek") {
    forAll(ParserGen.gen0, Arbitrary.arbitrary[String]) { (a, str) =>
      val pa = a.fa

      val left = pa.peek
      val right = pa.peek.peek

      val leftRes = left.parse(str)
      val rightRes = right.parse(str)
      assertEquals(leftRes, rightRes)
    }
  }

  property("a.backtrack.peek.orElse0(b.peek) == (a.backtrack.orElse0(b)).peek") {
    forAll(ParserGen.gen0, ParserGen.gen0, Arbitrary.arbitrary[String]) { (a, b, str) =>
      val pa = a.fa.backtrack
      val pb = b.fa

      val left = pa.peek.orElse0(pb.peek)
      val right = pa.orElse0(pb).peek

      val leftRes = left.parse(str).toOption
      val rightRes = right.parse(str).toOption
      assertEquals(leftRes, rightRes)
    }
  }

  property("a.peek == a.peek *> a.peek") {
    forAll(ParserGen.gen0, Arbitrary.arbitrary[String]) { (a, str) =>
      val pa = a.fa.peek

      val left = pa
      val right = pa *> pa

      val leftRes = left.parse(str)
      val rightRes = right.parse(str)
      assertEquals(leftRes, rightRes)
    }
  }

  property("!a == (!a) *> (!a)") {
    forAll(ParserGen.gen0, Arbitrary.arbitrary[String]) { (a, str) =>
      val pa = !a.fa

      val left = pa
      val right = pa *> pa

      val leftRes = left.parse(str)
      val rightRes = right.parse(str)
      assertEquals(leftRes, rightRes)
    }
  }

  property("!(!a) == a.peek") {
    forAll(ParserGen.gen0, Arbitrary.arbitrary[String]) { (a, str) =>
      val pa = a.fa

      val left = (!(!pa))
      val right = pa.peek

      val leftRes = left.parse(str).toOption
      val rightRes = right.parse(str).toOption
      assertEquals(leftRes, rightRes)
    }
  }

  property("!(!(!a)) == !a") {
    forAll(ParserGen.gen0, Arbitrary.arbitrary[String]) { (a, str) =>
      val pa = a.fa

      val left = !(!(!pa))
      val right = !pa

      val leftRes = left.parse(str).toOption
      val rightRes = right.parse(str).toOption
      assertEquals(leftRes, rightRes)
    }
  }

  property("!anyChar == end") {
    forAll { (str: String) =>
      val left = !Parser0.anyChar
      val right = Parser0.end

      val leftRes = left.parse(str).toOption
      val rightRes = right.parse(str).toOption
      assertEquals(leftRes, rightRes)
    }
  }

  property("!fail == unit") {
    forAll { (str: String) =>
      val left = !Parser0.fail
      val right = Parser0.unit

      val leftRes = left.parse(str)
      val rightRes = right.parse(str)
      assertEquals(leftRes, rightRes)
    }
  }

  property("!pure(_) == fail") {
    forAll { (str: String, i: Int) =>
      val left = !Parser0.pure(i)
      val right = Parser0.fail

      val leftRes = left.parse(str).toOption
      val rightRes = right.parse(str).toOption
      assertEquals(leftRes, rightRes)
    }
  }

  property("anyChar.repAs0[String] parses the whole string") {
    forAll { (str: String) =>
      assertEquals(Parser0.anyChar.repAs0[String].parse(str), Right(("", str)))
    }
  }

  property("string.soft ~ string is the same as concatenating the string") {
    forAll { (str1: String, str2: String, content: String) =>
      val left = (Parser0.string0(str1).soft ~ Parser0.string0(str2)).void
      val right = Parser0.string0(str1 + str2)

      val leftRes = left.parse(content).toOption
      val rightRes = right.parse(content).toOption
      assertEquals(leftRes, rightRes)

    }
  }

  property("Parser0.string(f).string == Parser0.string(f).as(f)") {
    forAll { (f: String) =>
      if (f.length > 1)
        assertEquals(Parser0.string(f).string, Parser0.string(f).as(f))

    }
  }

  property("char(c).as(c) == charIn(c)") {
    forAll { (c: Char) =>
      assertEquals(Parser0.char(c).as(c.toString), Parser0.char(c).string)
      assertEquals(Parser0.char(c).as(c), Parser0.charIn(c))
      assertEquals(Parser0.char(c).void.as(c), Parser0.charIn(c))
      assertEquals(Parser0.char(c).string.as(c), Parser0.charIn(c))
    }
  }

  property("select(pa.map(Left(_)))(pf) == (pa, pf).mapN((a, fn) => fn(a))") {
    forAll { (pa: Parser0[Int], pf: Parser0[Int => String], str: String) =>
      assertEquals(
        Parser0.select0(pa.map(Left(_)))(pf).parse(str),
        (pa, pf).mapN((a, f) => f(a)).parse(str)
      )
    }
  }

  property("select1(pa.map(Left(_)))(pf) == (pa, pf).mapN((a, fn) => fn(a))") {
    forAll { (pa: Parser[Int], pf: Parser0[Int => String], str: String) =>
      assertEquals(
        Parser0.select(pa.map(Left(_)))(pf).parse(str),
        (pa, pf).mapN((a, f) => f(a)).parse(str)
      )
    }
  }

  property("select(pa.map(Right(_)))(pf) == pa") {
    forAll { (pa: Parser0[String], pf: Parser0[Int => String], str: String) =>
      assertEquals(Parser0.select0(pa.map(Right(_)))(pf).parse(str), pa.parse(str))
    }
  }

  property("select1(pa.map(Right(_)))(pf) == pa") {
    forAll { (pa: Parser[String], pf: Parser0[Int => String], str: String) =>
      assertEquals(Parser0.select(pa.map(Right(_)))(pf).parse(str), pa.parse(str))
    }
  }

  property("p.filter(_ => true) == p") {
    forAll(ParserGen.gen0, Arbitrary.arbitrary[String]) { (genP, str) =>
      val res0 = genP.fa.filter(_ => true).parse(str)
      val res1 = genP.fa.parse(str)
      assertEquals(res0, res1)
    }
  }

  property("p.filter(_ => false) fails") {
    forAll(ParserGen.gen0, Arbitrary.arbitrary[String]) { (genP, str) =>
      val res = genP.fa.filter(_ => false).parse(str)
      assert(res.isLeft)
    }
  }

  property("select on pure values works as expected") {
    forAll { (left: Option[Either[Int, String]], right: Option[Int => String], str: String) =>
      val pleft = left match {
        case Some(e) => Parser0.pure(e)
        case None => Parser0.fail
      }

      val pright = right match {
        case Some(f) => Parser0.pure(f)
        case None => Parser0.fail
      }

      assertEquals(
        Parser0.select0(pleft)(pright).parse(str).toOption.map(_._2),
        left.flatMap {
          case Left(i) => right.map(_(i))
          case Right(s) =>
            // here even if right is None we have a result
            Some(s)
        }
      )
    }
  }

  property("mapFilter is the same as filter + map") {
    forAll { (pa: Parser0[Int], fn: Int => Option[String], str: String) =>
      val left = pa.mapFilter(fn)
      val right = pa.map(fn).filter(_.isDefined).map(_.get)

      assertEquals(left.parse(str), right.parse(str))
    }
  }

  property("mapFilter is the same as filter + map Parser") {
    forAll { (pa: Parser[Int], fn: Int => Option[String], str: String) =>
      val left = pa.mapFilter(fn)
      val right = pa.map(fn).filter(_.isDefined).map(_.get)

      assertEquals(left.parse(str), right.parse(str))
    }
  }

  property("collect is the same as filter + map") {
    forAll { (pa: Parser0[Int], fn: Int => Option[String], str: String) =>
      val left = pa.collect {
        case i if fn(i).isDefined => fn(i).get
      }
      val right = pa.map(fn).filter(_.isDefined).map(_.get)

      assertEquals(left.parse(str), right.parse(str))
    }
  }

  property("collect is the same as filter + map Parser") {
    forAll { (pa: Parser[Int], fn: Int => Option[String], str: String) =>
      val left = pa.collect {
        case i if fn(i).isDefined => fn(i).get
      }
      val right = pa.map(fn).filter(_.isDefined).map(_.get)

      assertEquals(left.parse(str), right.parse(str))
    }
  }
}
